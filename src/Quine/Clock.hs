{-# LANGUAGE CPP #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.Clock
  ( now
  , Time
  , DeltaTime
  ) where

import Control.Monad.IO.Class
#if ! MIN_VERSION_base(4,8,0)
import Data.Functor
#endif

type Time      = Double
type DeltaTime = Double

foreign import javascript unsafe "$r = Date.now();" js_now :: IO Time

-- | A simple monotonic system clock in seconds with undefined origin
now :: MonadIO m => m Time
now = liftIO js_now
