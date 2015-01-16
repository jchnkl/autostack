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

$( mkTransClass (\s -> take (length s - 1) s) "MonadFoo" "FooT" ["getFooT"] )
$( mkTransInstances (\s -> take (length s - 1) s) "MonadFoo" "FooT" ["getFooT"] )

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

$( mkTransClass (\s -> take (length s - 1) s) "MonadBar" "BarT" ["getBarT", "setBarT", "modifyBarT"] )
$( mkTransInstances (\s -> take (length s - 1) s) "MonadBar" "BarT" ["getBarT", "setBarT", "modifyBarT"] )

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

$( mkTransClass (\s -> take (length s - 1) s) "MonadFooBar" "FooBarT" ["getFooBarStringT", "raiseFooBarT"] )
$( mkTransInstances (\s -> take (length s - 1) s) "MonadFooBar" "FooBarT" ["getFooBarStringT", "raiseFooBarT"] )

example :: (MonadFoo m, MonadBar m, MonadFooBar m) => m ()
example = do
    getFoo >>= setBar . show
    raiseFooBar
    getFooBarString >>= setBar . show
    modifyBar (++ "foo")

components :: (Component FooT, Component BarT, Component FooBarT)
components = (fooComponent, barComponent, fooBarComponent)

$(mkRunComponents [| (fooComponent, barComponent, fooBarComponent) |])

main :: IO ()
main = runComponents components example
