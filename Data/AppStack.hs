{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

module Data.AppStack where

import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)

data Component t = forall d. Typeable d => Component
    { _startup :: MonadIO m => m d
    , _cleanup :: MonadIO m => d -> m ()
    , _retract :: MonadIO m => t m a -> d -> m a
    }
    deriving Typeable
