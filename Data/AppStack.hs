-- {-# LANGUAGE AllowAmbiguousTypes       #-}
-- {-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ExistentialQuantification       #-}
-- {-# LANGUAGE FunctionalDependencies       #-}
-- {-# LANGUAGE ImpredicativeTypes       #-}
-- {-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- {-# LANGUAGE StandaloneDeriving       #-}
-- {-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving       #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE RankNTypes       #-}
-- {-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE KindSignatures       #-}

module Data.AppStack where

-- import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
-- import Control.Monad.Catch -- (bracket)
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Data.Typeable



-- import Data.AppStack.TH

data Allocator t = forall r. Allocator
    { _acquire :: MonadIO m => m r
    , _release :: MonadIO m => r -> m ()
    , _run :: MonadIO m => t m a -> r -> m a
    }
