{-# LANGUAGE DeriveGeneric #-}
module Arbor.Network.StatsD.Options
( StatsTag(..)
, StatsConfig(..)
, SampleRate(..)
, statsConfigParser
)
where

import Arbor.Network.StatsD.Type (SampleRate (..))
import Data.Monoid               ((<>))
import Data.Text                 (Text)
import GHC.Generics              (Generic)
import Network.Socket            (HostName)
import Options.Applicative

import qualified Data.Text as Text

newtype StatsTag = StatsTag (Text, Text) deriving (Show, Eq, Generic)

data StatsConfig = StatsConfig
  { host       :: HostName
  , port       :: Int
  , tags       :: [StatsTag]
  , sampleRate :: SampleRate
  } deriving (Show, Generic)

statsConfigParser :: Parser StatsConfig
statsConfigParser = StatsConfig
  <$> strOption
    (  long "statsd-host"
    <> metavar "HOST_NAME"
    <> showDefault <> value "localhost"
    <> help "StatsD host name or IP address"
    <> hidden)
  <*> option auto
    (  long "statsd-port"
    <> metavar "PORT"
    <> showDefault <> value 8125
    <> help "StatsD port"
    <> hidden)
  <*> ( string2Tags <$> strOption
    (  long "statsd-tags"
    <> metavar "TAGS"
    <> showDefault <> value []
    <> help "StatsD tags"))
  <*> ( SampleRate <$> option auto
    (  long "statsd-sample-rate"
    <> metavar "SAMPLE_RATE"
    <> showDefault <> value 0.01
    <> help "StatsD sample rate"
    <> hidden))

string2Tags :: String -> [StatsTag]
string2Tags s = StatsTag . splitTag <$> splitTags
  where splitTags = Text.split (==',') (Text.pack s)
        splitTag t = Text.drop 1 <$> Text.break (==':') t
