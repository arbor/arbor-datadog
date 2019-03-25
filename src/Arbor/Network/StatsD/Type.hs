{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Arbor.Network.StatsD.Type where

import Control.Reaper          (Reaper)
import Data.BufferBuilder.Utf8
import Data.Text               (Text)
import Data.Time.Clock         (UTCTime)
import GHC.Generics
import Network.Socket          (HostName)
import System.IO               (Handle)

-- | Tags are a Datadog specific extension to StatsD. They allow you to tag a metric with a
-- dimension that’s meaningful to you and slice and dice along that dimension in your graphs.
-- For example, if you wanted to measure the performance of two video rendering algorithms,
-- you could tag the rendering time metric with the version of the algorithm you used.
newtype Tag = Tag
  { builder :: Utf8Builder ()
  } deriving (Generic)

newtype SampleRate = SampleRate Double
  deriving (Show, Eq, Ord, Generic)

newtype MetricName = MetricName
  { text :: Text
  } deriving (Show, Eq, Generic)

data MetricType = Gauge -- ^ Gauges measure the value of a particular thing at a particular time, like the amount of fuel in a car’s gas tank or the number of users connected to a system.
  | Counter    -- ^ Counters track how many times something happened per second, like the number of database requests or page views.
  | Timer      -- ^ StatsD only supports histograms for timing, not generic values (like the size of uploaded files or the number of rows returned from a query). Timers are essentially a special case of histograms, so they are treated in the same manner by DogStatsD for backwards compatibility.
  | Histogram  -- ^ Histograms track the statistical distribution of a set of values, like the duration of a number of database queries or the size of files uploaded by users. Each histogram will track the average, the minimum, the maximum, the median and the 95th percentile.
  | Set        -- ^ Sets are used to count the number of unique elements in a group. If you want to track the number of unique visitor to your site, sets are a great way to do that.
  deriving (Show, Eq, Generic)

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
  { name       :: !MetricName
  , sampleRate :: {-# UNPACK #-} !SampleRate
  , type_      :: !MetricType
  , value      :: !(Utf8Builder ())
  , tags       :: ![Tag]
  } deriving (Generic)

data Priority = Low | Normal
  deriving (Generic)

data AlertType = Error | Warning | Info | Success
  deriving (Generic)

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
  { title          :: {-# UNPACK #-} !Text
  , text           :: {-# UNPACK #-} !Text
  , dateHappened   :: !(Maybe UTCTime)
  , hostname       :: !(Maybe Text)
  , aggregationKey :: !(Maybe Text)
  , priority       :: !(Maybe Priority)
  , sourceTypeName :: !(Maybe Text)
  , alertType      :: !(Maybe AlertType)
  , tags           :: ![Tag]
  } deriving (Generic)


data ServiceCheckStatus = ServiceOk | ServiceWarning | ServiceCritical | ServiceUnknown
  deriving (Read, Show, Eq, Ord, Enum, Generic)

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
  { name         :: {-# UNPACK #-} !Text
  , status       :: !ServiceCheckStatus
  , message      :: !(Maybe Text)
  , dateHappened :: !(Maybe UTCTime)
  , hostname     :: !(Maybe Text)
  , tags         :: ![Tag]
  } deriving Generic

data DogStatsSettings = DogStatsSettings
  { host :: HostName -- ^ The hostname or IP of the DogStatsD server (default: 127.0.0.1)
  , port :: Int      -- ^ The port that the DogStatsD server is listening on (default: 8125)
  } deriving Generic

-- | Note that Dummy is not the only constructor, just the only publicly available one.
data StatsClient = StatsClient
  { handle :: !Handle
  , reaper :: Reaper (Maybe (Utf8Builder ())) (Utf8Builder ())
  , aspect :: MetricName
  , tags   :: [Tag]
  }
  | Dummy -- ^ Just drops all stats.
  deriving Generic

type EnvVarName = String
type TagKey = Text
