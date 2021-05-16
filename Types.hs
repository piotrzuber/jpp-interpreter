module Types where

import Data.List

data VarT = BoolT | IntT | StrT | VoidT deriving Eq

type ArgsT = [VarT]
type RetT = VarT
type FunT = (ArgsT, RetT)
mainSig :: FunT
mainSig = ([], IntT)

instance Show VarT where
    show BoolT = "bool"
    show IntT = "int"
    show StrT = "string"
    show VoidT = "void"

showFunArgsT :: [VarT] -> String
showFunArgsT argsT = "(" ++ (intercalate ", " $ map show argsT) ++ ")"

showFunT :: FunT -> String
showFunT (argsT, retT) = showFunArgsT argsT ++ " -> " ++ show retT

type Pos = Maybe (Int, Int)
showPos :: Pos -> String
showPos (Just (row, col)) = show row ++ ":" ++ show col
showPosMsg :: Pos -> String
showPosMsg p = " at " ++ showPos p ++ "."

type FunId = Ident
type ObjId = Ident
type VarId = Ident

data TCError 
    = AssTypeMismatch VarId VarT VarT Pos
    | DoubleObjDecl ObjId Pos
    | FunArgsTMismatch FunId ArgsT ArgsT Pos
    | MisplacedInterruption Pos
    | NoMain
    | NonBoolCondition VarT Pos
    | OperationTMismatch VarT VarT Pos
    | PrintArgTMismatch Pos
    | RetTMismatch FunId RetT RetT Pos
    | UndeclaredObj ObjId Pos
    | UnexpectedMainSig FunT Pos
    | UnexpectedNonVoidRet FunId RetT Pos
    | UnexpectedVoidRet FunId RetT Pos

showId :: Ident -> String
showId (Ident id) = id

instance Show TCError where
    show (AssTypeMismatch v expectedT t p) = "Type mismatch in assignment to " ++ showId v ++ ". Expected: " ++ show expectedT ++ ", got: " ++ show t ++ showPosMsg p
    show (DoubleObjDecl o p) = "Repeated declaration of " ++ showId o ++ showPosMsg p
    show (FunArgsTMismatch f expectedA a p) = "Arguments type mismatch when calling " ++ showId f ++ ". Expected: " ++ showFunArgsT expectedA ++ ", got: " ++ showFunArgsT a ++ showPosMsg p
    show (MisplacedInterruption p) = "Unexpected loop interruption " ++ showPosMsg p
    show NoMain = "Main function not declared."
    show (NonBoolCondition t p) = "Non-bool type of conditional expression. Got: " ++ show t ++ showPosMsg p
    show (OperationTMismatch expectedT t p) = "Operation type mismatch. Expected: " ++ show expectedT ++ ", got: " ++ show t ++ showPosMsg p
    show (PrintArgTMismatch p) = "Void expression used as a print argument " ++ showPosMsg p
    show (RetTMismatch f expectedT t p) = "Return type mismatch in " ++ showId f ++ " . Expected: " ++ show expectedT ++ ", got: " ++ show t ++ showPosMsg p
    show (UndeclaredObj o p) = "Reference to undeclared object -- " ++ showId o ++ showPosMsg p
    show (UnexpectedMainSig t p) = "Unexpected main function signature. Expected: " ++ showFunT mainSig ++ ", got: " ++ showFunT t ++ showPosMsg p
    show (UnexpectedNonVoidRet f t p) = "Non-void return in void declared function " ++ showId f ++ ". Got: " ++ show t ++ showPosMsg p
    show (UnexpectedVoidRet f t p) = "Void return in non-void declared function " ++ showId f ++ ". Expected: " ++ show t ++ showPosMsg p