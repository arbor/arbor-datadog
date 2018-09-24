{-# LANGUAGE ConstraintKinds #-}

module Arbor.Network.StatsD.Internal.Lens where

import qualified Data.Generics.Product.Fields as GL

type HasField'' n st ab = GL.HasField n st st ab ab
