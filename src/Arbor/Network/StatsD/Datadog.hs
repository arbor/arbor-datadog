{-| DogStatsD accepts custom application metrics points over UDP, and then periodically aggregates and forwards the metrics to Datadog, where they can be graphed on dashboards. The data is sent by using a client library such as this one that communicates with a DogStatsD server. -}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module Arbor.Network.StatsD.Datadog
  ( -- * Client interface
    Z.DogStatsSettings(..)
  , defaultSettings
  , createStatsClient
  , closeStatsClient
  , send
  , sendSampled
  , sendEvt
    -- * Data supported by DogStatsD
  , metric
  , Z.Metric
  , Z.MetricName
  , Z.MetricType
  , event
  , Z.Event
  , serviceCheck
  , Z.ServiceCheck
  , Z.ServiceCheckStatus
  , ToStatsD
    -- * Optional fields
  , Z.Tag
  , envTag
  , tag
  , tagged
  , sampled
  , sampled'
  , incCounter
  , addCounter
  , gauge
  , timer
  , histogram
  , ToMetricValue(..)
  , value
  , Z.SampleRate
  , Z.Priority(..)
  , Z.AlertType(..)
    -- * Dummy client
  , Z.StatsClient(Dummy)
) where

import Control.Applicative         ((<$>))
import Control.Lens
import Control.Monad               (when)
import Control.Monad.IO.Class
import Control.Reaper
import Data.BufferBuilder.Utf8
import Data.Generics.Product.Any
import Data.Generics.Product.Typed
import Data.List                   (intersperse)
import Data.Maybe                  (isNothing)
import Data.Semigroup              ((<>))
import Data.Text                   (Text)
import Data.Text.Encoding          (encodeUtf8)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Network.Socket              hiding (recv, recvFrom, send, sendTo)
import System.Environment
import System.IO                   (BufferMode (LineBuffering), IOMode (WriteMode), hClose, hSetBuffering)
import System.Random               (randomIO)

import qualified Arbor.Network.StatsD.Type as Z
import qualified Data.ByteString           as B
import qualified Data.Foldable             as F
import qualified Data.Text                 as T

epochTime :: UTCTime -> Int
epochTime = round . utcTimeToPOSIXSeconds

sampleAlways :: Z.SampleRate
sampleAlways = Z.SampleRate 1.0

cleanMetricText :: Text -> Text
cleanMetricText = T.map $ \c -> case c of
  ':' -> '_'
  '|' -> '_'
  '@' -> '_'
  _   -> c
{-# INLINE cleanMetricText #-}

escapeEventContents :: T.Text -> T.Text
escapeEventContents = T.replace "\n" "\\n"
{-# INLINE escapeEventContents #-}

-- | Create a tag from a key-value pair. Useful for slicing and dicing events in Datadog.
--
-- Key and value text values are normalized by converting ":"s, "|"s, and "@"s to underscores ("_").
tag :: Text -> Text -> Z.Tag
tag k v = Z.Tag (build k >> appendChar7 ':' >> build v)
  where
    build = appendText . cleanMetricText

-- | Converts a supported numeric type to the format understood by DogStatsD. Currently limited by BufferBuilder encoding options.
class ToMetricValue a where
  encodeValue :: a -> Utf8Builder ()

instance ToMetricValue Int where
  encodeValue = appendDecimalSignedInt

instance ToMetricValue Double where
  encodeValue = appendDecimalDouble

-- | Smart 'Metric' constructor. Use the lens functions to set the optional fields.
metric :: (ToMetricValue a) => Z.MetricName -> Z.MetricType -> a -> Z.Metric
metric n t v = Z.Metric n sampleAlways t (encodeValue v) []

-- | Special setter to update the value of a 'Metric'.
--
-- > metric ("foo"" :: Text) Counter (1 :: Int) & value .~ (5 :: Double)
value :: ToMetricValue a => Setter Z.Metric Z.Metric (Utf8Builder ()) a
value = sets $ \f m -> m { Z.value = encodeValue $ f $ Z.value m }
{-# INLINE value #-}

renderMetric :: Z.Metric -> Utf8Builder ()
renderMetric (Z.Metric n (Z.SampleRate sr) t v ts) = do
  appendText $ cleanMetricText $ n ^. the @"text"
  appendChar7 ':'
  v
  appendChar7 '|'
  unit
  formatRate
  formatTags
  where
    unit = case t of
      Z.Gauge     -> appendChar7 'g'
      Z.Counter   -> appendChar7 'c'
      Z.Timer     -> appendBS7 "ms"
      Z.Histogram -> appendChar7 'h'
      Z.Set       -> appendChar7 's'
    formatTags = case ts of
      [] -> return ()
      xs -> appendBS7 "|#" >> F.sequence_ (intersperse (appendChar7 ',') $ map (^. the @"builder") xs)
    formatRate = if sr == 1 then return () else appendBS7 "|@" >> appendDecimalDouble sr

-- | Smart 'Event' constructor. Use the lens functions to set the optional fields.
event :: Text -> Text -> Z.Event
event t d = Z.Event t d Nothing Nothing Nothing Nothing Nothing Nothing []

renderEvent :: Z.Event -> Utf8Builder ()
renderEvent e = do
  appendBS7 "_e{"
  encodeValue $ B.length escapedTitle
  appendChar7 ','
  encodeValue $ B.length escapedText
  appendBS7 "}:"
  -- This is safe because we encodeUtf8 below
  -- We do so to get the length of the ultimately encoded bytes for the datagram format
  unsafeAppendBS escapedTitle
  appendChar7 '|'
  -- This is safe because we encodeUtf8 below
  -- We do so to get the length of the ultimately encoded bytes for the datagram format
  unsafeAppendBS escapedText
  happened
  formatHostname
  aggregation
  formatPriority
  sourceType
  alert
  formatTags
  where
    escapedTitle :: B.ByteString
    escapedTitle = encodeUtf8 $ escapeEventContents $ e ^. the @"title"
    escapedText :: B.ByteString
    escapedText = encodeUtf8 $ escapeEventContents $ e ^. the @"text"
    makeField :: Foldable t => Char -> t (Utf8Builder b) -> Utf8Builder ()
    makeField c v = F.forM_ v $ \jv ->
      appendChar7 '|' >> appendChar7 c >> appendChar7 ':' >> jv
    cleanTextValue :: Functor f => (Z.Event -> f Text) -> f (Utf8Builder ())
    cleanTextValue f = (appendText . cleanMetricText) <$> f e
    -- TODO figure out the actual format that dateHappened values are supposed to have.
    happened :: Utf8Builder ()
    happened = F.forM_ (e ^. the @"dateHappened") $ \h -> do
      appendBS7 "|d:"
      appendDecimalSignedInt $ epochTime h
    formatHostname :: Utf8Builder ()
    formatHostname = makeField 'h' $ cleanTextValue (^. the @"hostname")
    aggregation :: Utf8Builder ()
    aggregation = makeField 'k' $ cleanTextValue (^. the @"aggregationKey")
    formatPriority :: Utf8Builder ()
    formatPriority = F.forM_ (e ^. the @"priority") $ \p -> do
      appendBS7 "|p:"
      appendBS7 $ case p of
        Z.Low    -> "low"
        Z.Normal -> "normal"
    sourceType :: Utf8Builder ()
    sourceType = makeField 's' $ cleanTextValue (^. the @"sourceTypeName")
    alert :: Utf8Builder ()
    alert = F.forM_ (e ^. the @"alertType") $ \a -> do
              appendBS7 "|t:"
              appendBS7 $ case a of
                Z.Error   -> "error"
                Z.Warning -> "warning"
                Z.Info    -> "info"
                Z.Success -> "success"
    formatTags :: Utf8Builder ()
    formatTags = case e ^. the @"tags" of
      [] -> return ()
      ts -> do
        appendBS7 "|#"
        sequence_ $ intersperse (appendChar7 ',') $ map (^. the @"builder") ts

serviceCheck :: Text -- ^ name
             -> Z.ServiceCheckStatus
             -> Z.ServiceCheck
serviceCheck n s = Z.ServiceCheck n s Nothing Nothing Nothing []

-- | Convert an 'Event', 'Metric', or 'StatusCheck' to their wire format.
class ToStatsD a where
  toStatsD :: a -> Utf8Builder ()

instance ToStatsD Z.Metric where
  toStatsD = renderMetric

instance ToStatsD Z.Event where
  toStatsD = renderEvent

instance ToStatsD Z.ServiceCheck where
  toStatsD check = do
    appendBS7 "_sc|"
    appendText $ cleanMetricText $ check ^. the @"name"
    appendChar7 '|'
    appendDecimalSignedInt $ fromEnum $ check ^. the @"status"
    F.forM_ (check ^. the @"message") $ \msg ->
      appendBS7 "|m:" >> appendText (cleanMetricText msg)
    F.forM_ (check ^. the @"dateHappened") $ \ts -> do
      appendBS7 "|d:"
      appendDecimalSignedInt $ epochTime ts
    F.forM_ (check ^. the @"hostname") $ \hn ->
      appendBS7 "|h:" >> appendText (cleanMetricText hn)
    case check ^. the @"tags" of
      [] -> return ()
      ts -> do
        appendBS7 "|#"
        sequence_ $ intersperse (appendChar7 ',') $ map (^. the @"builder") ts

defaultSettings :: Z.DogStatsSettings
defaultSettings = Z.DogStatsSettings "127.0.0.1" 8125

createStatsClient :: MonadIO m
                  => Z.DogStatsSettings
                  -> Z.MetricName
                  -> [Z.Tag]
                  -> m Z.StatsClient
createStatsClient s n ts = liftIO $ do
  addrInfos <- getAddrInfo (Just $ defaultHints { addrFlags = [AI_PASSIVE] })
                                    (Just $ s ^. the @"host")
                                    (Just $ show $ s ^. the @"port")
  case addrInfos of
    [] -> error "No address for hostname" -- TODO throw
    (serverAddr:_) -> do
      sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
      connect sock (addrAddress serverAddr)
      h <- socketToHandle sock WriteMode
      hSetBuffering h LineBuffering
      let builderAction work = do
            F.mapM_ (B.hPut h . runUtf8Builder) work
            return $ const Nothing
          reaperSettings = defaultReaperSettings { reaperAction = builderAction
                                                , reaperDelay = 1000000 -- one second
                                                , reaperCons = \item work -> Just $ maybe item (>> item) work
                                                , reaperNull = isNothing
                                                , reaperEmpty = Nothing
                                                }
      r <- mkReaper reaperSettings
      return $ Z.StatsClient h r n ts

closeStatsClient :: MonadIO m => Z.StatsClient -> m ()
closeStatsClient c = liftIO $ finalizeStatsClient c >> hClose (Z.handle c)

-- | Send a 'Metric', 'Event', or 'StatusCheck' to the DogStatsD server.
--
-- Since UDP is used to send the events,
-- there is no ack that sent values are successfully dealt with.
--
-- > withDogStatsD defaultSettings $ \client -> do
-- >   send client $ event "Wombat attack" "A host of mighty wombats has breached the gates"
-- >   send client $ metric "wombat.force_count" Gauge (9001 :: Int)
-- >   send client $ serviceCheck "Wombat Radar" ServiceOk
-- send :: (MonadBase IO m, ToStatsD v) => Z.StatsClient -> v -> m ()
-- send Dummy _             = return ()
-- send (StatsClient _ r) v = liftBase $ reaperAdd r (toStatsD v >> appendChar7 '\n')
-- {-# INLINEABLE send #-}

tagged :: (HasType [Z.Tag] v) => (a -> v) -> (a -> [Z.Tag]) -> a -> v
tagged getVal getTag a = getVal a & typed @[Z.Tag] %~ (getTag a ++)
{-# INLINE tagged #-}

sampled' :: (HasType Z.SampleRate v) => (a -> v) -> (a -> Z.SampleRate) -> a -> v
sampled' getVal getRate a = getVal a & typed @Z.SampleRate .~ getRate a
{-# INLINE sampled' #-}

sampled :: (HasType Z.SampleRate v) => (a -> v) -> Z.SampleRate -> a -> v
sampled f r a = f a & typed @Z.SampleRate .~ r
{-# INLINE sampled #-}

incCounter :: Z.MetricName -> Z.Metric
incCounter n = metric n Z.Counter (1 :: Int)
{-# INLINE incCounter #-}

addCounter :: Z.MetricName -> (a -> Int) -> a -> Z.Metric
addCounter n f a = metric n Z.Counter (f a)
{-# INLINE addCounter #-}

gauge :: ToMetricValue v => Z.MetricName -> (a -> v) -> a -> Z.Metric
gauge n f a = metric n Z.Gauge (f a)
{-# INLINE gauge #-}

timer :: ToMetricValue v => Z.MetricName -> (a -> v) -> a -> Z.Metric
timer n f a = metric n Z.Timer (f a)
{-# INLINE timer #-}

histogram :: ToMetricValue v => Z.MetricName -> (a -> v) -> a -> Z.Metric
histogram n f a = metric n Z.Histogram (f a)
{-# INLINE histogram #-}

send ::
  ( MonadIO m
  , ToStatsD v
  , HasType Z.MetricName v
  , HasType [Z.Tag] v)
  => Z.StatsClient
  -> v
  -> m ()
send Z.Dummy _                  = return ()
send (Z.StatsClient _ r n ts) v = liftIO $
  reaperAdd r ((toStatsD . addAspect n . addTags ts) v >> appendChar7 '\n')
{-# INLINEABLE send #-}

sendEvt :: (MonadIO m) => Z.StatsClient -> Z.Event -> m ()
sendEvt Z.Dummy _ = return ()
sendEvt (Z.StatsClient _ r (Z.MetricName n) ts) e = liftIO $
  reaperAdd r ((toStatsD . addTags (tag "aspect" n : ts)) e >> appendChar7 '\n')

sendSampled ::
  ( MonadIO m
  , ToStatsD v
  , HasType Z.SampleRate v
  , HasType Z.MetricName v
  , HasType [Z.Tag] v)
  => Z.StatsClient
  -> v
  -> m ()
sendSampled Z.Dummy _ = return ()
sendSampled c v     = liftIO $ do
  z <- Z.SampleRate <$> randomIO
  when (z <= v ^. typed @Z.SampleRate) $ send c v
{-# INLINEABLE sendSampled #-}

envTag :: Z.EnvVarName -> Z.TagKey -> IO (Maybe Z.Tag)
envTag var key = do
  mbVal <- lookupEnv var
  return $ (tag key . T.pack) <$> mbVal

finalizeStatsClient :: Z.StatsClient -> IO ()
finalizeStatsClient (Z.StatsClient h r _ _) = reaperStop r >>= F.mapM_ (B.hPut h . runUtf8Builder)
finalizeStatsClient Z.Dummy                 = return ()

addAspect :: (HasType Z.MetricName v) => Z.MetricName -> v -> v
addAspect (Z.MetricName a) v =
  if T.null a
    then v
    else v & typed @Z.MetricName %~ (\(Z.MetricName n) -> Z.MetricName (a <> "." <> n))
{-# INLINE addAspect #-}

addTags :: (HasType [Z.Tag] v) => [Z.Tag] -> v -> v
addTags [] v = v
addTags ts v = v & typed @[Z.Tag] %~ (ts ++)
{-# INLINE addTags #-}
