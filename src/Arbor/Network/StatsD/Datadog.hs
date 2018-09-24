{-| DogStatsD accepts custom application metrics points over UDP, and then periodically aggregates and forwards the metrics to Datadog, where they can be graphed on dashboards. The data is sent by using a client library such as this one that communicates with a DogStatsD server. -}

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Arbor.Network.StatsD.Datadog (
  -- * Client interface
  DogStatsSettings(..),
  defaultSettings,
  createStatsClient,
  closeStatsClient,
  send, sendSampled, sendEvt,
  -- * Data supported by DogStatsD
  metric,
  Metric,
  MetricName(..),
  MetricType(..),
  event,
  Event,
  serviceCheck,
  ServiceCheck,
  ServiceCheckStatus(..),
  ToStatsD,
  -- * Optional fields
  Tag,
  envTag, tag, tagged,
  sampled, sampled',
  incCounter, addCounter,
  gauge, timer, histogram,
  ToMetricValue(..),
  value,
  SampleRate(..),
  Priority(..),
  AlertType(..),
  HasName(..),
  HasSampleRate(..),
  HasType'(..),
  HasTags(..),
  HasTitle(..),
  HasText(..),
  HasDateHappened(..),
  HasHostname(..),
  HasAggregationKey(..),
  HasPriority(..),
  HasSourceTypeName(..),
  HasAlertType(..),
  HasHost(..),
  HasPort(..),
  HasStatus(..),
  HasMessage(..),
  -- * Dummy client
  StatsClient(Dummy)
) where

import Control.Applicative     ((<$>))
import Control.Lens
import Control.Monad           (when)
import Control.Monad.IO.Class
import Control.Reaper
import Data.BufferBuilder.Utf8
import Data.List               (intersperse)
import Data.Maybe              (isNothing)
import Data.Semigroup          ((<>))
import Data.Text               (Text)
import Data.Text.Encoding      (encodeUtf8)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Network.Socket          hiding (recv, recvFrom, send, sendTo)
import System.Environment
import System.IO               (BufferMode (LineBuffering), Handle, IOMode (WriteMode), hClose, hSetBuffering)
import System.Random           (randomIO)

import qualified Data.ByteString as B
import qualified Data.Foldable   as F
import qualified Data.Text       as T

epochTime :: UTCTime -> Int
epochTime = round . utcTimeToPOSIXSeconds

newtype SampleRate = SampleRate Double deriving (Show, Eq, Ord)
newtype MetricName = MetricName { fromMetricName :: Text } deriving (Show, Eq)

sampleAlways :: SampleRate
sampleAlways = SampleRate 1.0

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

-- | Tags are a Datadog specific extension to StatsD. They allow you to tag a metric with a
-- dimension that’s meaningful to you and slice and dice along that dimension in your graphs.
-- For example, if you wanted to measure the performance of two video rendering algorithms,
-- you could tag the rendering time metric with the version of the algorithm you used.
newtype Tag = Tag { fromTag :: Utf8Builder () }

-- | Create a tag from a key-value pair. Useful for slicing and dicing events in Datadog.
--
-- Key and value text values are normalized by converting ":"s, "|"s, and "@"s to underscores ("_").
tag :: Text -> Text -> Tag
tag k v = Tag (build k >> appendChar7 ':' >> build v)
  where
    build = appendText . cleanMetricText

data MetricType = Gauge      -- ^ Gauges measure the value of a particular thing at a particular time, like the amount of fuel in a car’s gas tank or the number of users connected to a system.
                | Counter    -- ^ Counters track how many times something happened per second, like the number of database requests or page views.
                | Timer      -- ^ StatsD only supports histograms for timing, not generic values (like the size of uploaded files or the number of rows returned from a query). Timers are essentially a special case of histograms, so they are treated in the same manner by DogStatsD for backwards compatibility.
                | Histogram  -- ^ Histograms track the statistical distribution of a set of values, like the duration of a number of database queries or the size of files uploaded by users. Each histogram will track the average, the minimum, the maximum, the median and the 95th percentile.
                | Set        -- ^ Sets are used to count the number of unique elements in a group. If you want to track the number of unique visitor to your site, sets are a great way to do that.
                deriving (Show, Eq)
-- | Converts a supported numeric type to the format understood by DogStatsD. Currently limited by BufferBuilder encoding options.
class ToMetricValue a where
  encodeValue :: a -> Utf8Builder ()

instance ToMetricValue Int where
  encodeValue = appendDecimalSignedInt

instance ToMetricValue Double where
  encodeValue = appendDecimalDouble

-- | Smart 'Metric' constructor. Use the lens functions to set the optional fields.
metric :: (ToMetricValue a) => MetricName -> MetricType -> a -> Metric
metric n t v = Metric n sampleAlways t (encodeValue v) []

-- | 'Metric'
--
-- The fields accessible through corresponding lenses are:
--
-- * 'name' @::@ 'MetricName'
--
-- * 'sampleRate' @::@ 'Double'
--
-- * 'type'' @::@ 'MetricType'
--
-- * 'value' @::@ 'ToMetricValue' @a => a@
--
-- * 'tags' @::@ @[@'Tag'@]@
data Metric = Metric
  { metricName       :: !MetricName
  , metricSampleRate :: {-# UNPACK #-} !SampleRate
  , metricType'      :: !MetricType
  , mValue           :: !(Utf8Builder ())
  , metricTags       :: ![Tag]
  }

makeFields ''Metric

-- | Special setter to update the value of a 'Metric'.
--
-- > metric ("foo"" :: Text) Counter (1 :: Int) & value .~ (5 :: Double)
value :: ToMetricValue a => Setter Metric Metric (Utf8Builder ()) a
value = sets $ \f m -> m { mValue = encodeValue $ f $ mValue m }
{-# INLINE value #-}

renderMetric :: Metric -> Utf8Builder ()
renderMetric (Metric n (SampleRate sr) t v ts) = do
  appendText $ cleanMetricText $ fromMetricName n
  appendChar7 ':'
  v
  appendChar7 '|'
  unit
  formatRate
  formatTags
  where
    unit = case t of
      Gauge     -> appendChar7 'g'
      Counter   -> appendChar7 'c'
      Timer     -> appendBS7 "ms"
      Histogram -> appendChar7 'h'
      Set       -> appendChar7 's'
    formatTags = case ts of
      [] -> return ()
      xs -> appendBS7 "|#" >> F.sequence_ (intersperse (appendChar7 ',') $ map fromTag xs)
    formatRate = if sr == 1 then return () else appendBS7 "|@" >> appendDecimalDouble sr

data Priority = Low | Normal
data AlertType = Error | Warning | Info | Success

-- | Smart 'Event' constructor. Use the lens functions to set the optional fields.
event :: Text -> Text -> Event
event t d = Event t d Nothing Nothing Nothing Nothing Nothing Nothing []

-- | 'Event'
--
-- The fields accessible through corresponding lenses are:
--
-- * 'title' @::@ 'Text'
--
-- * 'text' @::@ 'Text'
--
-- * 'dateHappened' @::@ 'Maybe' 'UTCTime'
--
-- * 'hostname' @::@ 'Maybe' 'Text'
--
-- * 'aggregationKey' @::@ 'Maybe' 'Text'
--
-- * 'priority' @::@ 'Maybe' 'Priority'
--
-- * 'sourceTypeName' @::@ 'Maybe' 'Text'
--
-- * 'alertType' @::@ 'Maybe' 'AlertType'
--
-- * 'tags' @::@ @[@'Tag'@]@
--
data Event = Event
  { eventTitle          :: {-# UNPACK #-} !Text
  , eventText           :: {-# UNPACK #-} !Text
  , eventDateHappened   :: !(Maybe UTCTime)
  , eventHostname       :: !(Maybe Text)
  , eventAggregationKey :: !(Maybe Text)
  , eventPriority       :: !(Maybe Priority)
  , eventSourceTypeName :: !(Maybe Text)
  , eventAlertType      :: !(Maybe AlertType)
  , eventTags           :: ![Tag]
  }

makeFields ''Event

renderEvent :: Event -> Utf8Builder ()
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
    escapedTitle = encodeUtf8 $ escapeEventContents $ eventTitle e
    escapedText = encodeUtf8 $ escapeEventContents $ eventText e
    makeField c v = F.forM_ v $ \jv ->
      appendChar7 '|' >> appendChar7 c >> appendChar7 ':' >> jv
    cleanTextValue f = (appendText . cleanMetricText) <$> f e
    -- TODO figure out the actual format that dateHappened values are supposed to have.
    happened = F.forM_ (eventDateHappened e) $ \h -> do
      appendBS7 "|d:"
      appendDecimalSignedInt $ epochTime h
    formatHostname = makeField 'h' $ cleanTextValue eventHostname
    aggregation = makeField 'k' $ cleanTextValue eventAggregationKey
    formatPriority = F.forM_ (eventPriority e) $ \p -> do
      appendBS7 "|p:"
      appendBS7 $ case p of
        Low    -> "low"
        Normal -> "normal"
    sourceType = makeField 's' $ cleanTextValue eventSourceTypeName
    alert = F.forM_ (eventAlertType e) $ \a -> do
              appendBS7 "|t:"
              appendBS7 $ case a of
                Error   -> "error"
                Warning -> "warning"
                Info    -> "info"
                Success -> "success"
    formatTags = case eventTags e of
      [] -> return ()
      ts -> do
        appendBS7 "|#"
        sequence_ $ intersperse (appendChar7 ',') $ map fromTag ts

data ServiceCheckStatus = ServiceOk | ServiceWarning | ServiceCritical | ServiceUnknown
  deriving (Read, Show, Eq, Ord, Enum)

-- | 'ServiceCheck'
--
-- The fields accessible through corresponding lenses are:
--
-- * 'name' @::@ 'Text'
--
-- * 'status' @::@ 'ServiceCheckStatus'
--
-- * 'message' @::@ 'Maybe' 'Text'
--
-- * 'dateHappened' @::@ 'Maybe' 'UTCTime'
--
-- * 'hostname' @::@ 'Maybe' 'Text'
--
-- * 'tags' @::@ @[@'Tag'@]@
data ServiceCheck = ServiceCheck
  { serviceCheckName         :: {-# UNPACK #-} !Text
  , serviceCheckStatus       :: !ServiceCheckStatus
  , serviceCheckMessage      :: !(Maybe Text)
  , serviceCheckDateHappened :: !(Maybe UTCTime)
  , serviceCheckHostname     :: !(Maybe Text)
  , serviceCheckTags         :: ![Tag]
  }

makeFields ''ServiceCheck

serviceCheck :: Text -- ^ name
             -> ServiceCheckStatus
             -> ServiceCheck
serviceCheck n s = ServiceCheck n s Nothing Nothing Nothing []

-- | Convert an 'Event', 'Metric', or 'StatusCheck' to their wire format.
class ToStatsD a where
  toStatsD :: a -> Utf8Builder ()

instance ToStatsD Metric where
  toStatsD = renderMetric

instance ToStatsD Event where
  toStatsD = renderEvent

instance ToStatsD ServiceCheck where
  toStatsD check = do
    appendBS7 "_sc|"
    appendText $ cleanMetricText $ check ^. name
    appendChar7 '|'
    appendDecimalSignedInt $ fromEnum $ check ^. status
    F.forM_ (check ^. message) $ \msg ->
      appendBS7 "|m:" >> appendText (cleanMetricText msg)
    F.forM_ (check ^. dateHappened) $ \ts -> do
      appendBS7 "|d:"
      appendDecimalSignedInt $ epochTime ts
    F.forM_ (check ^. hostname) $ \hn ->
      appendBS7 "|h:" >> appendText (cleanMetricText hn)
    case check ^. tags of
      [] -> return ()
      ts -> do
        appendBS7 "|#"
        sequence_ $ intersperse (appendChar7 ',') $ map fromTag ts

data DogStatsSettings = DogStatsSettings
  { dogStatsSettingsHost :: HostName -- ^ The hostname or IP of the DogStatsD server (default: 127.0.0.1)
  , dogStatsSettingsPort :: Int      -- ^ The port that the DogStatsD server is listening on (default: 8125)
  }

makeFields ''DogStatsSettings

defaultSettings :: DogStatsSettings
defaultSettings = DogStatsSettings "127.0.0.1" 8125

createStatsClient :: MonadIO m
                  => DogStatsSettings
                  -> MetricName
                  -> [Tag]
                  -> m StatsClient
createStatsClient s n ts = liftIO $ do
  addrInfos <- getAddrInfo (Just $ defaultHints { addrFlags = [AI_PASSIVE] })
                                    (Just $ s ^. host)
                                    (Just $ show $ s ^. port)
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
      return $ StatsClient h r n ts

closeStatsClient :: MonadIO m => StatsClient -> m ()
closeStatsClient c = liftIO $ finalizeStatsClient c >> hClose (statsClientHandle c)

-- | Note that Dummy is not the only constructor, just the only publicly available one.
data StatsClient = StatsClient
                   { statsClientHandle :: !Handle
                   , statsClientReaper :: Reaper (Maybe (Utf8Builder ())) (Utf8Builder ())
                   , statsAspect       :: MetricName
                   , statsTags         :: [Tag]
                   }
                 | Dummy -- ^ Just drops all stats.

-- | Send a 'Metric', 'Event', or 'StatusCheck' to the DogStatsD server.
--
-- Since UDP is used to send the events,
-- there is no ack that sent values are successfully dealt with.
--
-- > withDogStatsD defaultSettings $ \client -> do
-- >   send client $ event "Wombat attack" "A host of mighty wombats has breached the gates"
-- >   send client $ metric "wombat.force_count" Gauge (9001 :: Int)
-- >   send client $ serviceCheck "Wombat Radar" ServiceOk
-- send :: (MonadBase IO m, ToStatsD v) => StatsClient -> v -> m ()
-- send Dummy _             = return ()
-- send (StatsClient _ r) v = liftBase $ reaperAdd r (toStatsD v >> appendChar7 '\n')
-- {-# INLINEABLE send #-}

tagged :: (HasTags v [Tag]) => (a -> v) -> (a -> [Tag]) -> a -> v
tagged getVal getTag a = getVal a & tags %~ (getTag a ++)
{-# INLINE tagged #-}

sampled' :: (HasSampleRate v SampleRate) => (a -> v) -> (a -> SampleRate) -> a -> v
sampled' getVal getRate a = getVal a & sampleRate .~ getRate a
{-# INLINE sampled' #-}

sampled :: (HasSampleRate v SampleRate) => (a -> v) -> SampleRate -> a -> v
sampled f r a = f a & sampleRate .~ r
{-# INLINE sampled #-}

incCounter :: MetricName -> Metric
incCounter n = metric n Counter (1 :: Int)
{-# INLINE incCounter #-}

addCounter :: MetricName -> (a -> Int) -> a -> Metric
addCounter n f a = metric n Counter (f a)
{-# INLINE addCounter #-}

gauge :: ToMetricValue v => MetricName -> (a -> v) -> a -> Metric
gauge n f a = metric n Gauge (f a)
{-# INLINE gauge #-}

timer :: ToMetricValue v => MetricName -> (a -> v) -> a -> Metric
timer n f a = metric n Timer (f a)
{-# INLINE timer #-}

histogram :: ToMetricValue v => MetricName -> (a -> v) -> a -> Metric
histogram n f a = metric n Histogram (f a)
{-# INLINE histogram #-}

send :: (MonadIO m, ToStatsD v, HasName v MetricName, HasTags v [Tag])
     => StatsClient
     -> v
     -> m ()
send Dummy _                  = return ()
send (StatsClient _ r n ts) v = liftIO $
  reaperAdd r ((toStatsD . addAspect n . addTags ts) v >> appendChar7 '\n')
{-# INLINEABLE send #-}

sendEvt :: (MonadIO m) => StatsClient -> Event -> m ()
sendEvt Dummy _ = return ()
sendEvt (StatsClient _ r (MetricName n) ts) e = liftIO $
  reaperAdd r ((toStatsD . addTags (tag "aspect" n : ts)) e >> appendChar7 '\n')

sendSampled :: (MonadIO m, ToStatsD v, HasSampleRate v SampleRate, HasName v MetricName, HasTags v [Tag])
            => StatsClient
            -> v
            -> m ()
sendSampled Dummy _ = return ()
sendSampled c v     = liftIO $ do
  z <- SampleRate <$> randomIO
  when (z <= v ^. sampleRate) $ send c v
{-# INLINEABLE sendSampled #-}

type EnvVarName = String
type TagKey = T.Text

envTag :: EnvVarName -> TagKey -> IO (Maybe Tag)
envTag var key = do
  mbVal <- lookupEnv var
  return $ (tag key . T.pack) <$> mbVal

finalizeStatsClient :: StatsClient -> IO ()
finalizeStatsClient (StatsClient h r _ _) = reaperStop r >>= F.mapM_ (B.hPut h . runUtf8Builder)
finalizeStatsClient Dummy                 = return ()

addAspect :: (HasName v MetricName) => MetricName -> v -> v
addAspect (MetricName a) v =
  if T.null a
    then v
    else v & name %~ (\(MetricName n) -> MetricName (a <> "." <> n))
{-# INLINE addAspect #-}

addTags :: (HasTags v [Tag]) => [Tag] -> v -> v
addTags [] v = v
addTags ts v = v & tags %~ (ts ++)
{-# INLINE addTags #-}
