{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

-- import Control.Monad.State hiding (get)
-- import Control.Monad.Trans.Free
-- import Control.Monad.Trans.Maybe
-- import Control.Monad.Trans.State
import Data.Map (Map)
-- import Data.Maybe
import GHC.Exts (Constraint)
-- import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Catch -- (bracket)
import Control.Monad.Reader
-- import Control.Monad.Trans.Reader hiding (ask, asks)
import Control.Monad.State
import Data.Typeable

import Graphics.XHB

import Unsafe.Coerce
import Language.Haskell.TH

import Data.AppStack.TH

{-
newtype ResourceT m a = ResourceT { unResourceT :: StateT (Int, String) m a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadTrans, Typeable)

class ResourceGetter r where
    getter :: (Int, String) -> r

instance ResourceGetter Int where
    getter (i, _) = i

instance ResourceGetter String where
    getter (_, s) = s

class ResourceSetter r where
    setter :: r -> (Int, String) -> (Int, String)

instance ResourceSetter Int where
    setter i (_, s) = (i, s)

instance ResourceSetter String where
    setter s (i, _) = (i, s)

getResource :: (ResourceGetter r, Monad m) => ResourceT m r
getResource = ResourceT $ gets getter

setResource :: (ResourceSetter r, Monad m) => r -> ResourceT m ()
setResource r = ResourceT $ modify (setter r)
-}


data Allocator t = forall r. Allocator
    { _acquire :: MonadIO m => m r
    , _release :: MonadIO m => r -> m ()
    -- , _run :: n a -> r -> m a
    -- , _runResource :: t m a -> r -> m (a, r)
    -- , _runResource :: Monad m => r -> (t m a -> m a)
    -- , _runResource :: forall m n a. MonadIO m => ResourceT r n m a -> r -> m a
    -- , _wrap :: MonadIO m => r -> ResourceT r m ()
    -- , _lift :: MonadIO m => t m a -> r -> m a
    -- , _run :: forall t m a. (ResourceClass r m, MonadIO m) => t m a -> r -> ResourceT m a
    , _run :: MonadIO m => t m a -> r -> m a
    }


someThing :: MonadIO m => FooT (BarT m) ()
someThing = do
    getFoo >>= liftIO . print
    lift getBar >>= liftIO . print
    lift $ setBar "asdf"
    lift getBar >>= liftIO . print
    return ()

coolStuff :: (MonadIO m) => m ()
-- coolStuff :: (MonadIO m, MonadTrans t, Monad (t m)) => t m ()
coolStuff = do
    -- _ <- liftResource getBar
    -- liftResource getFoo >>= liftResource . setBar . show
    -- lifty getFoo >>= liftIO . print
    return ()

coolStuff' :: (MonadIO m) => FooT m ()
coolStuff' = coolStuff

runCoolStuff :: MonadIO m => m ()
runCoolStuff = flip runReaderT (0::Int) . unFooT $ coolStuff

-- ||   runAllocator :: forall (t :: (* -> *) -> * -> *)
-- ||                          (t1 :: (* -> *) -> * -> *)
-- ||                          (t2 :: (* -> *) -> * -> *)
-- ||                          (m :: * -> *)
-- ||                          c.
-- ||                   (MonadIO (t2 m), MonadIO (t1 (t2 m)), MonadIO m, MonadMask m) =>
-- ||                   (Allocator t, Allocator t1, Allocator t2) -> t (t1 (t2 m)) c -> m c
-- runAllocator :: (MonadIO m, MonadMask m)
--              -- => (Allocator FooT Int, Allocator BarT String)
--              => (Allocator FooT, Allocator BarT, Allocator BazT)
--              -> FooT (BarT (BazT m)) a
--              -- -> ReaderT Int (StateT String (ReaderT String m)) a
--              -- -> (forall n. (MonadFoo n, MonadBar n, MonadIO n) => n a)
--              -> m a
runAllocator (Allocator a1 r1 rr1, Allocator a2 r2 rr2, Allocator a3 r3 rr3) f = do
    bracket a1 r1 $ \rs1 -> do
        bracket a2 r2 $ \rs2 -> do
            bracket a3 r3 $ \rs3 -> do
                flip rr3 rs3 $ flip rr2 rs2 $ flip rr1 rs1 $ f



newtype FooT m a = FooT { unFooT :: ReaderT Int m a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadTrans, Typeable)

getFoo :: Monad m => FooT m Int
getFoo = FooT ask

-- class Monad m => MonadFoo m where
--     getFoo' :: m Int

-- instance Monad m => MonadFoo (FooT m) where
--     getFoo' = getFoo

-- instance (MonadTrans t, MonadFoo m, Monad (t m)) => MonadFoo (t m) where
--     getFoo' = lift getFoo'

exampleAllocator_1 :: Allocator FooT
exampleAllocator_1 = Allocator
    { _acquire = liftIO (print "acquire 1") >> return (42::Int)
    , _release = \_ -> liftIO (print "release 1") >> return ()
    , _run = runReaderT . unFooT
    }




newtype BarT m a = BarT { unBarT :: StateT String m a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadTrans, Typeable)

getBar :: Monad m => BarT m String
getBar = BarT get

setBar :: Monad m => String -> BarT m ()
setBar = BarT . put

modifyBar :: Monad m => (String -> String) -> BarT m ()
modifyBar = BarT . modify

exampleAllocator_2 :: Allocator BarT
exampleAllocator_2 = Allocator
    { _acquire = liftIO (print "acquire 2") >> return ("exampleAllocator_2"::String)
    , _release = \_ -> liftIO (print "release 2") >> return ()
    , _run = evalStateT . unBarT
    }




newtype BazT m a = BazT { unBazT :: ReaderT String m a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadTrans, Typeable)

bazGetter :: Monad m => BazT m String
bazGetter = BazT ask

class Monad m => MonadBaz m where
    bazGetter' :: m String

instance (MonadTrans t, MonadBaz m, Monad (t m)) => MonadBaz (t m) where
    bazGetter' = lift bazGetter'

instance Monad m => MonadBaz (BazT m) where
    bazGetter' = bazGetter

exampleAllocator_3 :: Allocator BazT
exampleAllocator_3 = Allocator
    { _acquire = liftIO (print "acquire 3") >> return ("exampleAllocator_3"::String)
    , _release = \_ -> liftIO (print "release 3") >> return ()
    , _run = runReaderT . unBazT
    }

allocators = (exampleAllocator_1, exampleAllocator_2, exampleAllocator_3)



elStupido0 :: Monad m => Int -> (Int -> String) -> (String -> Int -> BarT m a) -> BarT m a
elStupido0 = undefined

elStupido1 :: Monad m => Int -> (Int -> String) -> (String -> Int -> m a) -> BarT m a
elStupido1 = undefined

elStupido2 :: Monad m => Int -> String -> Int -> m a -> BarT m a
elStupido2 = undefined

elStupido3 :: Monad m => Int -> (String -> String) -> BarT m Int
elStupido3 = undefined

elStupido4 :: Int -> String -> Int
elStupido4 = undefined

-- (AppT (AppT ArrowT (ConT GHC.Types.Int))
--       (AppT (AppT ArrowT (ConT GHC.Base.String))
--             (AppT (AppT ArrowT
--                         (ConT GHC.Types.Int))
--                   (AppT (AppT ArrowT (AppT (VarT m_1627501762) (VarT a_1627501763)))
--                         (AppT (AppT (ConT DynaMon.SharedResource.BarT) (VarT m_1627501762)) (VarT a_1627501763)))
--             )
--       ))


$(mkRunAllocators [| allocators |])

-- $( fmap (:[]) $ mkTransClass "MonadBar" "BarT" [ ("getBar'", "getBar")
--                                                , ("setBar'", "setBar")
--                                                ] )

-- $( fmap (:[]) $ mkTransInstances "MonadBar" "BarT" [ ("getBar'", "getBar")
--                                                   -- , ("setBar'", "setBar")
--                                                   ] )

$( mkTransClass (++"'") "MonadFoo" "FooT" ["getFoo"] )

$( mkTransInstances (++"'") "MonadFoo" "FooT" ["getFoo"] )

yoBar :: MonadFoo m => String -> BarT m ()
yoBar = undefined

-- $( mkTransClass (++"'") "MonadBar" "BarT"
--     [ "getBar"
--     , "setBar"
--     , "modifyBar"
--     -- , "yoBar"
--     -- , "elStupido3"
--     ] )

-- $( mkTransInstances (++"'") "MonadBar" "BarT"
--     [ "getBar"
--     , "setBar"
--     , "modifyBar"
--     -- , "yoBar"
--     -- , "elStupido3"
--     ] )

-- withBar :: Monad m => (String -> m a) -> BarT m a
-- withBar f = getBar >>= lift . f

class Monad m => MonadBar m where
    getBar' :: m String
    setBar' :: String -> m ()
    modifyBar' :: (String -> String) -> m ()
    yoBar' :: MonadFoo m => String -> m ()

instance (MonadTrans t, MonadBar m, Monad (t m)) => MonadBar (t m) where
    getBar' = lift getBar'
    setBar' = lift . setBar'
    modifyBar' = lift . modifyBar'
    -- yoBar' = lift . yoBar'

instance Monad m => MonadBar (BarT m) where
    getBar' = getBar
    setBar' = setBar
    modifyBar' = modifyBar
    -- yoBar' = yoBar

-- instance (MonadFoo m, Monad (t m), MonadTrans t) => MonadFoo (t m)
--     where getFoo' = lift getFoo'

-- instance (MonadFoo m, Monad (t m), MonadTrans t) => MonadFoo (t m)
--     where getFoo' = lift getFoo
-- instance (Monad m, Monad (t m), MonadTrans t) => MonadFoo (t m)
--     where getFoo' = lift getFoo

someThing' :: (MonadIO m, MonadFoo m, MonadBar m, MonadBaz m) => m ()
someThing' = do
    getFoo' >>= liftIO . putStrLn . ("getFoo': " ++) . show
    getBar' >>= liftIO . putStrLn . ("getBar': " ++) . show
    bazGetter' >>= liftIO . putStrLn . ("bazGetter': " ++) . show
    setBar' "asdf"
    getBar' >>= liftIO . putStrLn . ("getBar': " ++) . show
    return ()


-- $(mkRunAllocators [| (exampleAllocator_1, exampleAllocator_2) |])

-- resourceTest :: MonadResource r m => m ()
-- resourceTest = undefined -- intResourceTest >> stringResourceTest

-- runAllocators :: (MonadIO m, MonadMask m)
--               => (Allocator Int, Allocator String)
--               -> (forall n. MonadResource r n => n a)
--               -> m a
-- runAllocators = undefined
-- runAllocators (Allocator a1 r1 , Allocator a2 r2 ) f = do
    -- r <- ask
    -- liftIO $ bracket a1 r1 $ \rs1 -> do
    --     liftIO $ bracket a2 r2 $ \rs2 -> do
    --         flip runReaderT r $ evalStateT f (rs1, rs2)



-- runAllocators :: forall m a. (MonadIO m, MonadMask m, MonadReader Int m)
--               => (Allocator Int, Allocator String)
--               -> (forall n. (MonadReader Int n, MonadState (Int, String) n) => n a)
--               -> m a
-- runAllocators (Allocator a1 r1 , Allocator a2 r2 ) f = do
--     r <- ask
--     liftIO $ bracket a1 r1 $ \rs1 -> do
--         liftIO $ bracket a2 r2 $ \rs2 -> do
--             flip runReaderT r $ evalStateT f (rs1, rs2)



-- $(mkInstances ''ResourceGetterClass ''Resources)

-- $(mkMapTupleN 0)
-- $(mkMapTupleN 1)
-- $(mkMapTupleN 2)
-- $(mkMapTupleN 3)

-- $(mkSequenceTupleN 0)
-- $(mkSequenceTupleN 1)
-- $(mkSequenceTupleN 2)
-- $(mkSequenceTupleN 3)
