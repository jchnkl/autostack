{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.AppStack.TH where

import Data.Maybe
import Language.Haskell.TH

{-
tupleTypes :: Name -> Q (Int, [Type])
tupleTypes name = do
    reify name >>= \case
        TyConI (TySynD _ _ appt) -> return $ types 1 [] appt
        info -> error (show info ++ " not handled")

    where
    types :: Int -> [Type] -> Type -> (Int, [Type])
    types n conts (AppT (TupleT _) cont) = (n, cont : conts)
    types n conts (AppT appt       cont) = types (n+1) (cont : conts) appt
    types _ _     typeT                  = error (show typeT ++ " not handled")
-}

{-
mkInstances :: Name -> Name -> Q [Dec]
mkInstances name = fmap (uncurry mk) . tupleTypes
    where
    mk n ts = mk' n (1::Int) ts
    mk' _ _ []     = []
    mk' n n' (t:ts) = InstanceD [] (AppT (ConT name) t) [fun n n'] : mk' n (n'+1) ts

    fun n n' = FunD (mkName "getResource") [Clause [args n] (body n') []]

    args n = TupP . reverse . map (VarP . mkName) $ vars n

    vars 0 = []
    vars n = ("a" ++ show n) : vars (n-1)

    body n = NormalB (VarE (mkName $ "a" ++ show n))
-}

{-
mkMapTupleFun :: Int -> Dec
mkMapTupleFun c = FunD id_ [Clause ([in_] ++ fun) (NormalB out) []]
    where
    nthVar a n = mkName $ a ++ show n
    id_ = mkName $ "mapTuple" ++ show c
    in_ = TupP [VarP (nthVar "i" n) | n <- [1..c]]
    fun = [VarP (nthVar "f" n) | n <- [1..c]]
    out = TupE [AppE (VarE $ nthVar "f" n) (VarE $ nthVar "i" n) | n <- [1..c]]

mkMapTupleSig :: Int -> Dec
mkMapTupleSig c = sig
    where
    i n = mkName $ "i" ++ show n
    o n = mkName $ "o" ++ show n
    id_ = mkName $ "mapTuple" ++ show c
    sig = SigD id_ (ForallT tyvars [] $ AppT (AppT ArrowT (tup i c)) (typ 1))
    tyvars = [PlainTV (i n) | n <- [1..c]] ++ [PlainTV (o n) | n <- [1..c]]
    fun n = AppT (AppT ArrowT (VarT $ i n)) (VarT $ o n)
    tup v 1 = AppT (TupleT c)    (VarT $ v (1::Int))
    tup v n = AppT (tup v (n-1)) (VarT $ v n)
    typ n
        | n == c    = AppT (AppT ArrowT (fun n)) (tup o c)
        | otherwise = AppT (AppT ArrowT (fun n)) (typ (n+1))

-- mapTuple3 :: (a1, a2, a3) -> (a1 -> b1) -> (a2 -> b2) -> (a3 -> b3) -> (b1, b2, b3)
mkMapTupleN :: Int -> Q [Dec]
mkMapTupleN n
    | n <= 0    = error "mapTuple0 not supported"
    | otherwise = return $ [mkMapTupleSig n, mkMapTupleFun n]
-}

{-
mkSequenceTupleFun :: Int -> Dec
mkSequenceTupleFun c = FunD id_ [Clause [in_] (NormalB out) []]
    where
    nthVar a n = mkName $ a ++ show n
    id_ = mkName $ "sequenceTuple" ++ show c
    returnE = VarE (mkName "return")
    in_ = TupP [VarP (nthVar "ma" n) | n <- [1..c]]
    out = DoE $ binds ++ [nobinds]
    binds = [BindS (VarP $ nthVar "a" n) (VarE $ nthVar "ma" n) | n <- [1..c]]
    nobinds = NoBindS (AppE returnE tup)
    tup = TupE [VarE $ nthVar "a" n | n <- [1..c]]

-- sequenceTuple3 :: Monad m => (m t, m t1, m t2) -> m (t, t1, t2)
mkSequenceTupleSig :: Int -> Dec
mkSequenceTupleSig c = sig
    where

    m = mkName "m"
    a n = mkName $ "a" ++ show n
    at n = VarT $ mkName $ "a" ++ show n
    mat n = AppT (VarT m) (at n)
    -- o n = mkName $ "o" ++ show n

    id_ = mkName $ "sequenceTuple" ++ show c

    sig = SigD id_ (ForallT tyvars ctxt fun)
    ctxt = [ClassP (mkName "Monad") [VarT m]]
    tyvars = PlainTV m : [PlainTV (a n) | n <- [1..c]]

    fun = AppT (AppT ArrowT (tup mat c)) (AppT (VarT m) (tup at c))

    tup v 1 = AppT (TupleT c)    (v (1::Int))
    tup v n = AppT (tup v (n-1)) (v n)

mkSequenceTupleN :: Int -> Q [Dec]
mkSequenceTupleN n
    | n <= 0    = error "sequenceTuple0 not supported"
    | otherwise = return $ [mkSequenceTupleSig n, mkSequenceTupleFun n]
-}

-- | allocators = (myAllocator_1, myAllocator_2, ..)
-- | putStrLn $( stringE . show =<< ppr <$> mkRunAllocators [| allocators  |] )
mkRunAllocators :: ExpQ -> Q [Dec]
mkRunAllocators expq = runQ expq >>= \case
    VarE name -> reify name >>= fromInfo
    TupE tups -> return [mkFun $ length tups]
    _         -> printError

    where
    printError                = expq >>= error . ("invalid expression: " ++) . show

    fromInfo (VarI _ typ _ _) = tupLength typ >>= \n -> return [mkFun n]
    fromInfo                _ = printError

    tupLength (TupleT  n)     = return n
    tupLength (AppT appt _)   = tupLength appt
    tupLength           _     = printError

    -- F: Function
    -- N: Name
    -- E: Expression
    -- P: Partial

    fargN           = mkName "f"
    appFN           = mkName "$"
    flipFN          = mkName "flip"
    bracketN        = mkName "bracket"
    allocCtorN      = mkName "Allocator"
    nthN v n        = mkName $ v ++ show n

    -- (Allocator a1 r1 rr1, Allocator a2 r2 rr2, ..)
    allocVarPs n    = [VarP (nthN "a" n), VarP (nthN "r" n), VarP (nthN "rr" n)]
    allocCtor n     = ConP allocCtorN (allocVarPs n)

    -- bracket a1 r1 $ \rs1 -> bracket a2 r2 $ \rs2 -> ..
    bracketPF n     = AppE (AppE (VarE bracketN) (VarE $ nthN "a" n)) (VarE $ nthN "r" n)
    bracketE n e    = UInfixE (bracketPF n) (VarE appFN) (LamE [VarP $ nthN "rs" n] e)

    -- build the bracket recursively
    mkBracketE n' n
        | n > n'    = runF n'
        | otherwise = bracketE n $ mkBracketE n' (n+1)

    -- .. $ flip rr2 rs2 $ flip rr1 rs1 $ f
    runPF n         = (AppE (AppE (VarE flipFN) (VarE $ nthN "rr" n)) (VarE $ nthN "rs" n))
    runF 0          = VarE fargN
    runF n          = UInfixE (runPF n) (VarE appFN) (runF (n-1))

    -- runAllocators (Allocator ..) f = do
    funN            = mkName "runAllocators"
    funPat n        = TupP $ [allocCtor n' | n' <- [1..n]]
    funBody n       = NormalB (DoE [NoBindS $ mkBracketE n 1])
    funClause n     = Clause [funPat n, VarP fargN] (funBody n) []
    mkFun n         = FunD funN [funClause n]



type ClassName = String
type TransName = String
type MethodName = String
type FunctionName = String

mkTransClass :: (FunctionName -> MethodName)
             -> ClassName -- ^ class Monad m => ClassName m where ..
             -> TransName -- ^ instance Monad m => ClassName (TransName m) where ..
             -> [FunctionName] -- ^ where mname_1 = fname_1; mname_2 = fname_2 ..
             -> Q [Dec]
mkTransClass toMethod cn tn fns = mapM classDecs fns >>= return . (:[]) . classd

    where
    m = mkName "m"
    cname = mkName cn
    ctxt = [ClassP (mkName "Monad") [VarT m]]
    classd = ClassD ctxt cname [PlainTV m] []

    printError msg = error $ "cannot generate class method: " ++ msg -- show (ppr typ)

    classDecs fn = methodSig fn >>= return . SigD (mkName $ toMethod fn)

    methodSig fn = reify (mkName fn) >>= \case
        VarI _ typ _ _ -> maybe (printError $ show $ ppr typ) return $ scrapFunctionType typ
        info -> printError $ fn ++ " (info: " ++ show info ++ ")"

    scrapFunctionType = scrapTrans . scrapForall

    scrapForall (ForallT _ _ typ) = typ
    scrapForall typ               = typ

    scrapTrans (AppT t1 t2)
        | isTrans t1 = Just $ replaceMonad t2
        | isTrans t2 = Just $ replaceMonad t1
        | otherwise  = Just $ AppT (fromMaybe t1 $ scrapTrans t1) (fromMaybe t2 $ scrapTrans t2)
    scrapTrans _ = Nothing

    replaceMonad _ = VarT m

    isTrans (ConT name) = nameBase name == tn
    isTrans _           = False

mkTransInstances :: (FunctionName -> MethodName) -> ClassName -> TransName -> [FunctionName] -> Q [Dec]
mkTransInstances toMethod cn tn fns = sequence [liftInstance, bottomInstance]
    where
    m = mkName "m"
    t = mkName "t"
    liftN = mkName "lift"
    cname = mkName cn
    tname = mkName tn

    monad_cn_m_Cxt = classP (mkName cn) [varT m]
    monad_m_Cxt = classP (mkName "Monad") [(varT m)]
    monad_tm_Cxt = classP (mkName "Monad") [appT (varT t) (varT m)]
    monadTransCxt = classP (mkName "MonadTrans") [varT t]

    -- instance (MonadTrans t, MonadFoo m, Monad (t m)) => ClassName (t m) where
    liftCxt      = cxt [monad_cn_m_Cxt, monad_tm_Cxt, monadTransCxt]

    liftBodyArgs f 1 = appE (varE $ mkName f) (varE $ mkName "a1")
    liftBodyArgs f n = appE (liftBodyArgs f (n-1)) (varE $ mkName $ "a" ++ show n)

    liftBody f 0 = normalB $ appE (varE liftN) (varE $ mkName f)
    liftBody f n = normalB $ appE (varE liftN) (parensE $ liftBodyArgs f n)

    liftClause f = do
        n <- argCount f
        let args = [mkName ("a" ++ show i) | i <- [(1::Int)..n]]
        clause [varP p | p <- args] (liftBody f n) []

    liftFunDs    = [funD (mkName (toMethod fn)) [liftClause (toMethod fn)] | fn <- fns]
    liftInstance = instanceD liftCxt (appT (conT cname) (appT (varT t) (varT m))) liftFunDs

    argCount fname = reify (mkName fname) >>= \case
        VarI _ ty _ _ -> return $ countFunArgs ty
        ClassOpI _ ty _ _ -> return $ countFunArgs ty
        info -> error $ "no args for " ++ show info

    -- instance Monad m => ClassName (TransName m) where
    bottomCxt = cxt [monad_m_Cxt]
    bottomClause f = clause [] (normalB $ varE $ mkName f) []
    bottomFunDs = [funD (mkName (toMethod fn)) [bottomClause fn] | fn <- fns]
    bottomInstance = instanceD bottomCxt (appT (conT cname) (appT (conT tname) (varT m))) bottomFunDs

countFunArgs :: Num a => Type -> a
countFunArgs (ForallT _ _ t) = countFunArgs t
countFunArgs (AppT ArrowT _) = 1
countFunArgs (AppT t1 t2)    = countFunArgs t1 + countFunArgs t2
countFunArgs _               = 0
