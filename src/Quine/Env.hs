{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.Env
  ( Env(..)
  , HasEnv(..)
  , frameCounter
  , heightGauge
  , widthGauge
  ) where

import Control.Lens
import Quine.Options

-- * Environment

data Env = Env
  { _envOptions   :: Options
  , _frameCounter :: Counter
  , _widthGauge   :: Gauge
  , _heightGauge  :: Gauge
  }

makeLenses ''Env

instance HasOptions Env where
  options = envOptions

class HasOptions t => HasEnv t where
  env :: Lens' t Env

instance HasEnv Env where
  env = id
