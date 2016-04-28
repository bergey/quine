{-# LANGUAGE CPP #-}
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
module Quine.Options
  ( Options(..)
  , HasOptions(..)
  , pointScale
  ) where

import Control.Applicative
import Control.Lens
import Data.Default
import Prelude hiding (init)

data Options = Options
  { _optionsFullScreen       :: !Bool
  , _optionsFullScreenNormal :: !Bool
  , _optionsHighDPI          :: !Bool
  , _optionsHighDPIRatio     :: !Float
  , _optionsWindowWidth      :: !Int
  , _optionsWindowHeight     :: !Int
  , _optionsFragment         :: !FilePath
  , _optionsDebug            :: !Bool
  }

makeClassy ''Options

instance Default Options where
  def = Options False False False 2.0 1024 768 "shaders/generators.frag" False

pointScale :: Options -> Float
pointScale opts = if opts^.optionsHighDPI then opts^.optionsHighDPIRatio else 1
