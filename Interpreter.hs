module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except

import qualified Data.Map as Map
import Data.Maybe

import AbsGrammar
import ErrM
import Types

data Store = Store {
    freeLoc :: VarLoc,
    store :: Map.Map VarLoc VarV
}

data Fn = Fn [FArgPassType] ([FArg] -> Interpreter VarV)

type FunEnv = Map.Map FunId Fn
type VarEnv = Map.Map VarId VarLoc
data IEnv = IEnv {
    funEnv :: FunEnv,
    varEnv :: VarEnv
}

type Interpreter = ReaderT IEnv (StateT Store (ExceptT RTError IO))

updateFunEnv :: FunId -> Fn -> IEnv -> IEnv
updateFunEnv fId f (IEnv fe ve) = IEnv (Map.insert fId f fe) ve

updateVarEnv :: VarId -> VarLoc -> IEnv -> IEnv
updateVarEnv vId l (IEnv fe ve) = IEnv fe (Map.insert vId l ve)

setDefVal :: Type -> Store -> (VarLoc, Store)
setDefVal Bool (Store l s) = (l, Store (l + 1) (Map.insert l (bool defValues) s))
setDefVal Int (Store l s) = (l, Store (l + 1) (Map.insert l (int defValues) s))
setDefVal Str (Store l s) = (l, Store (l + 1) (Map.insert l (str defValues) s))

setLocVal :: VarLoc -> VarV -> Store -> Store
setLocVal l v (Store freeL s) = Store freeL (Map.insert l v s)

evalExpV :: Expr -> Interpreter VarV
evalExpV (EVar vId) = evalVarV vId
evalExpV (ELitInt i) = return $ IntV i
evalExpV ELitTrue = return $ BoolV True
evalExpV ELitFalse = return $ BoolV False
evalExpV (EApp fId args) = evalFunV fId args
evalExpV (EString s) = return $ StrV s
evalExpV (Neg e) = getNegationV e
evalExpV (Not e) = getNegationV e
evalExpV (EMul e1 op e2) = getMultiplicationV op e1 e2
evalExpV (EAdd e1 op e2) = getAdditionV op e1 e2
evalExpV (ERel e1 op e2) = getComparisonV op e1 e2
evalExpV (EAnd e1 e2) = evalConjunctionV e1 e2
evalExpV (EOr e1 e2) = evalAlternativeV e1 e2

evalVarV :: VarId -> Interpreter VarV
evalVarV vId = do
    env <- ask
    st <- get
    return $ fromJust $ Map.lookup (fromJust (Map.lookup vId (varEnv env))) (store st)

evalFunV :: FunId -> [Expr] -> Interpreter VarV
evalFunV fId args = do 
    env <- ask
    let Fn passTypes fun = fromJust $ Map.lookup fId (funEnv env)
    args <- mapM getFArgFromExpr (zip args passTypes)
    fun args

getFArgFromExpr :: (Expr, FArgPassType) -> Interpreter FArg
getFArgFromExpr (EVar vId, RefPass) = do
    env <- ask
    return $ Ref $ fromJust $ Map.lookup vId (varEnv env)
getFArgFromExpr (_, RefPass) = throwError BadRefException
getFArgFromExpr (e, ValPass) = do
    eV <- evalExpV e
    return $ Val eV

getNegationV :: Expr -> Interpreter VarV
getNegationV e = do
    eV <- evalExpV e 
    case eV of
        BoolV b -> return $ BoolV (not b)
        IntV i -> return $ IntV (-i)
        _ -> error "Wrong negation type" 

getMultiplicationV :: MulOp -> Expr -> Expr -> Interpreter VarV
getMultiplicationV op e1 e2 = do
    eV1 <- evalExpV e1
    eV2 <- evalExpV e2
    case (eV1, eV2) of
        (IntV i1, IntV i2) -> case op of
            Div -> if i2 == 0
                then throwError $ DivideByZeroEx
                else return $ IntV (i1 `div` i2)
            Mod -> if i2 == 0
                then throwError $ DivideByZeroEx
                else return $ IntV (i1 `mod` i2)
            Times -> return $ IntV (i1 * i2)
        _ -> error "Wrong expression type for multiplication"

getAdditionV :: AddOp -> Expr -> Expr -> Interpreter VarV
getAdditionV op e1 e2 = do 
    eV1 <- evalExpV e1
    eV2 <- evalExpV e2
    case (eV1, eV2) of 
        (IntV i1, IntV i2) -> case op of
            Minus -> return $ IntV (i1 - i2)
            Plus -> return $ IntV (i1 + i2)
        _ -> error "Wrong expression type for addition"

getComparisonV :: RelOp -> Expr -> Expr -> Interpreter VarV
getComparisonV op e1 e2 = do
    eV1 <- evalExpV e1
    eV2 <- evalExpV e2
    case (eV1, eV2) of 
        (IntV i1, IntV i2) -> case op of
            LTH -> return $ BoolV (i1 < i2)
            LE -> return $ BoolV (i1 <= i2)
            GTH -> return $ BoolV (i1 > i2)
            GE -> return $ BoolV (i1 >= i2)
            EQU -> return $ BoolV (i1 == i2)
            NE -> return $ BoolV (i1 /= i2)
        _ -> error "Wrong expression type for comparison"

evalConjunctionV :: Expr -> Expr -> Interpreter VarV
evalConjunctionV e1 e2 = do 
    eV1 <- evalExpV e1
    eV2 <- evalExpV e2
    case (eV1, eV2) of 
        (BoolV b1, BoolV b2) -> return $ BoolV (b1 && b2)
        _ -> error "Wrong expression type for conjunction"

evalAlternativeV :: Expr -> Expr -> Interpreter VarV
evalAlternativeV e1 e2 = do
    eV1 <- evalExpV e1
    eV2 <- evalExpV e2
    case (eV1, eV2) of 
        (BoolV b1, BoolV b2) -> return $ BoolV (b1 || b2)
        _ -> error "Wrong expression type for alternative"

evalStmtV :: Stmt -> Interpreter (Maybe VarV)
evalStmtV Empty = return Nothing
evalStmtV (BStmt b) = do 
    env <- ask
    local (const env) (evalBlockV b)
evalStmtV (Ass vId e) = performAss vId e >> return Nothing
evalStmtV (Ret e) = do 
    retV <- evalExpV e 
    return $ Just retV
evalStmtV VRet = return $ Just VoidV
evalStmtV (Cond e s) = evalCondV e s
evalStmtV (CondElse e s1 s2) = evalCondElseV e s1 s2
evalStmtV w@(While e s) = do
    eV <- evalExpV e 
    case eV of 
        BoolV True -> do
            sRet <- evalStmtV s
            case sRet of 
                Just BreakS -> return Nothing
                Just ContinueS -> evalStmtV w
                Nothing -> evalStmtV w
                Just varV -> return $ Just varV
        _ -> return Nothing 
evalStmtV (Interrupt i) = case i of
    Break -> return $ Just BreakS
    Continue -> return $ Just ContinueS
evalStmtV (SExp e) = evalExpV e >> return Nothing
evalStmtV (Print e) = performPrint e >> return Nothing

evalBlockV :: Block -> Interpreter (Maybe VarV)
evalBlockV (Blk ds ss) = do
    env <- handleDeclsI ds
    local (const env) (evalStmtsV ss)

evalStmtsV :: [Stmt] -> Interpreter (Maybe VarV)
evalStmtsV stmts = do
    foldM foldStmts Nothing stmts

foldStmts :: Maybe VarV -> Stmt -> Interpreter (Maybe VarV)
foldStmts Nothing stmt = evalStmtV stmt
foldStmts (Just sV) _ = return (Just sV)

performAss :: VarId -> Expr -> Interpreter ()
performAss vId e = do 
    eV <- evalExpV e
    env <- ask
    let Just loc = Map.lookup vId (varEnv env)
    st <- get
    put $ Store (freeLoc st) (Map.insert loc eV (store st))

evalCondV :: Expr -> Stmt -> Interpreter (Maybe VarV)
evalCondV e s = do 
    eV <- evalExpV e
    let BoolV b = eV
    if b 
        then evalStmtV s
        else return Nothing

evalCondElseV :: Expr -> Stmt -> Stmt -> Interpreter (Maybe VarV)
evalCondElseV e s1 s2 = do
    eV <- evalExpV e
    let BoolV b = eV
    if b
        then evalStmtV s1
        else evalStmtV s2

performPrint :: Expr -> Interpreter ()
performPrint e = do 
    eV <- evalExpV e 
    liftIO $ putStrLn $ show eV
    return ()

handleDeclsI :: [Decl] -> Interpreter IEnv
handleDeclsI [] = ask
handleDeclsI (dhead : dtail) = do
    env <- handleDeclI dhead
    local (const env) (handleDeclsI dtail)

handleDeclI :: Decl -> Interpreter IEnv
handleDeclI (FnDecl fT fId args b) = do 
    env <- ask
    let fun argsL = do
        let envFunId = updateFunEnv fId (Fn (map getFArgsPassTypeFromArg args) fun) env
        envFunArgs <- local (const envFunId) $ handleArgs args argsL
        retV <- local (const envFunArgs) $ evalBlockV b
        case (fromMaybe VoidV retV, fT) of
            (VoidV, Void) -> return VoidV
            (VoidV, _) -> throwError $ MissingRetStmt fId
            (ret, _) -> return ret
    return $ updateFunEnv fId (Fn (map getFArgsPassTypeFromArg args) fun) env
handleDeclI (VarDecl vT vId) = do
    env <- ask
    st <- get
    let (l, newStore) = setDefVal vT st
    put newStore
    return $ updateVarEnv vId l env

handleArgs :: [Arg] -> [FArg] -> Interpreter IEnv
handleArgs (a : _) [] = error "Function call argument count mismatch"
handleArgs [] (fa: _) = error "Function call argument count mismatch"
handleArgs [] [] = ask
handleArgs (argDesc : argDescs) (passedArg : passedArgs) = do 
    env <- handleArg argDesc passedArg
    local (const env) (handleArgs argDescs passedArgs)

handleArg :: Arg -> FArg -> Interpreter IEnv
handleArg (RefArg vT vId) (Ref l) = do
    env <- ask
    return $ updateVarEnv vId l env
handleArg (ValArg vT vId) (Val v) = do
    env <- ask
    st <- get
    let (l, newStore) = setDefVal vT st
    put $ setLocVal l v newStore
    return $ updateVarEnv vId l env
handleArg (ValArg _ _) (Ref _) = error "Function argument pass-type mismatch"
handleArg (RefArg _ _) (Val _) = error "Function argument pass-type mismatch"

getFArgsPassTypeFromArg :: Arg -> FArgPassType
getFArgsPassTypeFromArg (RefArg _ _) = RefPass
getFArgsPassTypeFromArg (ValArg _ _) = ValPass

evalProg :: Program -> Interpreter VarV
evalProg (Prog ds) = do
    env <- handleDeclsI ds
    local (const env) (evalFunV (Ident "main") [])

interpret :: Program -> ExceptT RTError IO VarV
interpret p = do
    (ret, s) <- runStateT (runReaderT (evalProg p) (IEnv Map.empty Map.empty)) (Store 0 Map.empty)
    return ret
