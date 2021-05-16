module TypeCheck where

import Data.List
import qualified Data.Map as Map
import Data.Maybe

import AbsGrammar
import ErrM
import Types

import Control.Monad.Except
import Control.Monad.Reader 
import Control.Monad.Trans.Except

getVarT :: Type -> VarT
getVarT Bool = BoolT
getVarT Int = IntT
getVarT Str = StrT
getVarT Void = VoidT

getFunT :: Type -> FunT
getFunT (Fun ret args) = (map getVarT args, getVarT ret)

type VarTEnv = Map.Map VarId (VarT, Int)
type FunTEnv = Map.Map FunId (FunT, Int)
data Env = Env {
    block :: Int, 
    loop :: Bool,
    funEnv :: FunTEnv,
    varEnv :: VarTEnv
} deriving Show

incBlock :: Env -> Env
incBlock (Env b l fe ve) = Env (b + 1) l fe ve

enableLoop :: Env -> Env
enableLoop (Env b _ fe ve) = Env b True fe ve

disableLoop :: Env -> Env
disableLoop (Env b _ fe ve) = Env b False fe ve

updateFunTEnv :: FunId -> FunT -> Env -> Env
updateFunTEnv fId fT (Env b l fe ve) = Env b l (Map.insert fId (fT, b) fe) ve

updateVarTEnv :: VarId -> VarT -> Env -> Env
updateVarTEnv vId vT (Enc b l fe ve) = Env b l fe (Map.insert vId (vT, b) ve)

type TC = ReaderT Env (ExceptT TCError IO)

evalExpT :: Expr -> TC VarT
evalExpT (EVar vId) = getVarTFromEnv vId
evalExpT (ELitInt _) = return IntT
evalExpT ELitTrue = return BoolT
evalExpT ELitFalse = return BoolT
evalExpT (EApp fId args) = getAppTFromEnv fId args
evalExpT (EString _) = return StrT
evalExpT (Neg exp) = ensureExpT exp IntT >> return IntT
evalExpT (Not exp) = ensureExpT exp BoolT >> reutrn BoolT
evalExpT (EMul exp1 _ exp2) = ensureExpT exp1 IntT >> ensureExpT exp2 IntT >> return IntT
evalExpT (EAdd exp1 _ exp2) = ensureExpT exp1 IntT >> ensureExpT exp2 IntT >> return IntT
evalExpT (ERel exp1 _ exp2) = ensureExpT exp1 IntT >> ensureExpT exp2 IntT >> return BoolT
evalExpT (EAnd exp1 exp2) = ensureExpT exp1 BoolT >> ensureExpT exp2 BoolT >> return BoolT
evalExpT (EOr exp1 exp2) = ensureExpT exp1 BoolT >> ensureExpT exp2 BoolT >> reutrn BoolT

ensureT :: VarT -> VarT -> TC ()
ensureT vt expectedT = unless (vt == expectedT) $ throwError $ OperationTMismatch expectedT vt

ensureExpT :: Expr -> VarT -> TC ()
ensureExpT e expectedT = do
    expT <- evalExpT e
    ensureT expT expectedT

getVarTFromEnv :: VarId -> TC VarT
getVarTFromEnv vId = do
    env <- ask
    case Map.lookup vId (varEnv env) of
        Just (vT, _) -> return vT
        Nothing -> throwError $ UndeclaredObj vId

getFunTFromEnv :: FunId -> TC FunT
getFunTFromEnv fId = do
    env <- ask
    case Map.lookup fId (funEnv env) of 
        Just (fT, _) -> return fT
        Nothing -> throwError $ UndeclaredObj fId

getAppTFromEnv :: FunId -> [Expr] -> TC VarT
getAppTFromEnv fId argsEx = do
    (argsExpectedT, retT) <- getFunTFromEnv fId
    argsT <- MapM evalExpT argsEx
    if not (argsExpectedT == argsT)
        then throwError $ FunArgsTMismatch fId argsExpectedT argsT
        else return retT

evalStmtT :: Stmt -> TC (Maybe VarT)
evalStmtT Empty = return Nothing
evalStmtT (BStmt block) = do
    env <- ask 
    local (const $ incBlock env) (evalBlockT block)
evalStmtT (Ass vId e) = ensureAssT vId e >> return Nothing
evalStmtT (Ret e) = evalRetT e
evalStmtT VRet = return (Just VoidT)
evalStmtT (Cond e s) = evalCondT e s
evalStmtT (CondElse e s1 s2) = evalCondElseT e s1 s2
evalStmtT (While e s) = evalWhileT e s
evalStmtT (Interrupt i) = ensureInterrupt i >> return Nothing
evalStmtT (SExp e) = evalExpT e >> return Nothing
evalStmtT (Print e) = ensurePrintT e >> return Nothing

ensureAssT :: VarId -> Expr -> TC ()
ensureAssT vId e = do
    vT <- getVarTFromEnv vId
    eT <- evalExpT e
    unless (eT == vT) $ throwError $ AssTypeMismatch vId vT eT

evalBlockT :: Block -> TC (Maybe VarT)
evalBlockT (Blk ds ss) = do 
    env <- handleDecls ds
    ssRetUnfiltered <- local (const env) (mapM evalStmtT ss)
    let ssRet = filter (not . isNothing) ssRetUnfiltered
    if null ssRet
        then return Nothing
        else if and $ map (== head ssRet) (tail ssRet)
            then return $ head ssRet
            else throwError VagueBlockT

getTFromArg :: Arg -> Type
getTFromArg (RefArg t _) = t
getTFromArg (ValArg t _) = t

castArgToVarDecl :: Arg -> VarDecl
castArgToVarDecl (RefArg t i) = VarDecl t i
castArgToVarDecl (ValArg t i) = VarDecl t i 

handleDecl :: Decl -> TC Env
handleDecl (FnDecl rT fId args b) = do 
    env <- ask
    let fType = Fun rT (map getTFromArg args)
    envFunId <- case Map.lookup fId (funEnv env) of
        Just (funT, bl) -> if bl == block env
            then throwError $ DoubleObjDecl fId
            else return $ updateFunTEnv fId (getFunT fType) env
        Nothing -> return $ updateFunTEnv fId (getFunT fType) env
    let setUpLocalFunEnv = incBlock $ disableLoop envFunId
    localFunEnv <- local (const setUplocalFunEnv) (checkDecls $ map castArgToVarDecl args)
    blockT <- local (const localFunEnv) $ evalBlockT b
    ensureFunRetT fId (fromMaybe VoidT blockT) (getVarT retType)
    return envFunId
handleDecl (VarDecl vT vId) = do
    env <- ask
    case Map.lookup vId (varEnv env) of
        Just (varT, bl) -> if bl == block env
            then throwError $ DoubleVarDecl varIdent pos
            else return $ updateVarTEnv vId (getVarT vT) env
        Nothing -> return $ updateVarTEnv vId (getVarT vT) env

handleDecls :: [Decl] -> TC Env
handleDecls [] = ask
handleDecls (dhead : dtail) = do
    env <- handleDecl dhead
    local (const env) (handleDecls dtail)

evalRetExpT :: Expr -> TC (Maybe VarT)
evalRetExpT exp = do
    expT <- evalExpT exp
    if VoidT == expT
        then throwError $ UnexpectedVoidExpRet
        else return (Just expT)

ensureFunRetT :: FunId -> VarT -> VarT -> TC ()
ensureFunRetT fId t expectedT = unless (t == expectedT) $ throwError $ RetTMismatch fId expectedT t

evalCondT :: Expr -> Stmt -> TC (Maybe VarT)
evalCondT e s = do
    eT <- evalExpT e
    if (BoolT = eT)
        then evalStmtT s
        else throwError $ NonBoolCondition eT

evalCondElseT :: Expr -> Stmt -> Stmt -> TC (Maybe VarT)
evalCondElseT e s1 s2 = do
    eT <- evalExpT e
    if (BoolT = eT)
        then do
            sT1 <- evalStmtT s1
            sT2 <- evalStmtT s2
            case (sT1, sT2) of
                (_, Nothing) -> return sT1
                (Nothing, _) -> return sT2
                (_, _) -> if sT1 == sT2
                    then return sT1
                    else throwError IfElseVagueRetT

evalWhileT :: Expr -> Stmt -> TC (Maybe VarT)
evalWhileT e s = do
    eT <- evalExpT e
    if BoolT == eT
        then do
            env <- ask
            local (const $ enableLoop env) (evalStmtT s)
        else throwError $ NonBoolCondition eT

ensureInterrupt :: Inter -> TC ()
ensureInterrupt i = do
    env <- ask
    unless (loop env) $ throwError MisplacedInterruption

ensurePrintT :: Expr -> TC ()
ensurePrintT e = do
    eT <- evalExpT e
    when (VoidT == eT) $ throwError PrintArgTMismatch

evalProgram :: Program -> TC ()
evalProgram (Prog ds) = do
    env <- handleDecls ds
    case Map.lookup (Ident "main") (funEnv env) of 
        Just (fT, _) -> unless (fT == mainFunT) $ throwError $ UnexpectedMainFunT fT
        Nothing -> throwError NoMain

typeCheck :: Program -> TC ()
typeCheck p = runReaderT (evalProgram p) (Env 0 False Map.empty Map.empty)