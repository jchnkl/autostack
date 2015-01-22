{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverlappingInstances       #-}

module Data.AutoStack.Example where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Typeable
import Data.AutoStack
import Data.AutoStack.TH
import Data.AutoStack.Example.Util

newtype FooT m a = FooT { unFooT :: ReaderT Int m a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadTrans, Typeable)

fooComponent :: Component FooT
fooComponent = Component
    { _acquire = liftIO (print "acquire FooT") >> return (0::Int)
    , _release = \_ -> liftIO (print "release FooT")
    , _retract = runReaderT . unFooT
    }

getFooT :: Monad m => FooT m Int
getFooT = FooT ask

-- generate class and instances for `FooT`
mkTransClass dropLast "MonadFoo" "FooT" ["getFooT"]
mkTransInstances dropLast "MonadFoo" "FooT" ["getFooT"]

newtype BarT m a = BarT { unBarT :: StateT String m a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadTrans, Typeable)

barComponent :: Component BarT
barComponent = Component
    { _acquire = liftIO (print "acquire BarT") >> return "bar"
    , _release = \_ -> liftIO (print "release BarT")
    , _retract = evalStateT . unBarT
    }

getBarT :: Monad m => BarT m String
getBarT = BarT get

setBarT :: Monad m => String -> BarT m ()
setBarT = BarT . put

modifyBarT :: Monad m => (String -> String) -> BarT m ()
modifyBarT = BarT . modify

-- generate class and instances for `BarT`
mkTransClass dropLast "MonadBar" "BarT" ["getBarT", "setBarT", "modifyBarT"]
mkTransInstances dropLast "MonadBar" "BarT" ["getBarT", "setBarT", "modifyBarT"]

newtype FooBarT m a = FooBarT { unFooBarT :: ReaderT String (StateT Int m) a }
    deriving (Applicative, Functor, Monad, MonadIO, Typeable)

instance MonadTrans FooBarT where
    lift = FooBarT . lift . lift

fooBarComponent :: Component FooBarT
fooBarComponent = Component
    { _acquire = liftIO (print "acquire FooBarT") >> return ("foobar", 0::Int)
    , _release = \_ -> liftIO (print "release FooBarT")
    , _retract = \m (r, s) -> flip evalStateT s . flip runReaderT r . unFooBarT $ m
    }

getFooBarStringT :: Monad m => FooBarT m String
getFooBarStringT = FooBarT ask

raiseFooBarT :: Monad m => FooBarT m ()
raiseFooBarT = FooBarT $ lift $ modify (+1)

-- generate class and instances for `FooBarT`
mkTransClass dropLast "MonadFooBar" "FooBarT" ["getFooBarStringT", "raiseFooBarT"]
mkTransInstances dropLast "MonadFooBar" "FooBarT" ["getFooBarStringT", "raiseFooBarT"]

components :: (Component FooT, Component BarT, Component FooBarT)
components = (fooComponent, barComponent, fooBarComponent)

-- generate `runWithComponents` function
mkRunWithComponents 'components

example :: (MonadFoo m, MonadBar m, MonadFooBar m) => m ()
example = do
    getFoo >>= setBar . show
    raiseFooBar
    getFooBarString >>= setBar . show
    modifyBar (++ "foo")

main :: IO ()
main = runWithComponents example
