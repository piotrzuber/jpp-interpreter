module Types where

import Data.List

import AbsGrammar

data VarT = BoolT | IntT | StrT | VoidT deriving Eq

type ArgsT = [VarT]
type RetT = VarT
type FunT = (ArgsT, RetT)
mainFunT :: FunT
mainFunT = ([], IntT)

instance Show VarT where
    show BoolT = "bool"
    show IntT = "int"
    show StrT = "string"
    show VoidT = "void"

showFunArgsT :: [VarT] -> String
showFunArgsT argsT = "(" ++ (intercalate ", " $ map show argsT) ++ ")"

showFunT :: FunT -> String
showFunT (argsT, retT) = showFunArgsT argsT ++ " -> " ++ show retT

data VarV = BoolV Bool | IntV Integer | StrV String | VoidV
instance Show VarV where
    show (BoolV v) = show v
    show (IntV v) = show v
    show (StrV v) = v
    show VoidV = error "Attempt to print void value"

type FunId = Ident
type ObjId = Ident
type VarId = Ident

type VarLoc = Int

data DefV = DefV {
    bool :: VarV,
    int :: VarV,
    str :: VarV
}
defValues :: DefV
defValues = DefV (BoolV False) (IntV 0) (StrV "")

data FArgPassType = RefPass | ValPass
data FArg = Ref VarLoc | Val VarV

showId :: Ident -> String
showId (Ident id) = id

data TCError 
    = AssTypeMismatch VarId VarT VarT
    | DoubleObjDecl ObjId
    | FunArgsTMismatch FunId ArgsT ArgsT
    | IfElseVagueRetT
    | MisplacedInterruption
    | NoMain
    | NonBoolCondition VarT
    | OperationTMismatch VarT VarT
    | PrintArgTMismatch
    | RetTMismatch FunId RetT RetT
    | UndeclaredObj ObjId
    | UnexpectedMainFunT FunT
    | UnexpectedNonVoidRet FunId RetT
    | UnexpectedVoidExpRet
    | VagueBlockT

instance Show TCError where
    show (AssTypeMismatch v expectedT t) = "Type mismatch in assignment to " ++ showId v ++ ". Expected: " ++ show expectedT ++ ", got: " ++ show t
    show (DoubleObjDecl o) = "Repeated declaration of " ++ showId o
    show (FunArgsTMismatch f expectedA a) = "Arguments type mismatch when calling " ++ showId f ++ ". Expected: " ++ showFunArgsT expectedA ++ ", got: " ++ showFunArgsT a
    show IfElseVagueRetT = "Ambigous return type in conditional statement"
    show MisplacedInterruption = "Unexpected loop interruption"
    show NoMain = "Main function not declared"
    show (NonBoolCondition t) = "Non-bool type of conditional expression. Got: " ++ show t
    show (OperationTMismatch expectedT t) = "Operation type mismatch. Expected: " ++ show expectedT ++ ", got: " ++ show t
    show PrintArgTMismatch = "Void expression used as a print argument"
    show (RetTMismatch f expectedT t) = "Return type mismatch in " ++ showId f ++ " . Expected: " ++ show expectedT ++ ", got: " ++ show t
    show (UndeclaredObj o) = "Reference to undeclared object -- " ++ showId o
    show (UnexpectedMainFunT t) = "Unexpected main function signature. Expected: " ++ showFunT mainFunT ++ ", got: " ++ showFunT t
    show (UnexpectedNonVoidRet f t) = "Non-void return in void declared function " ++ showId f ++ ". Got: " ++ show t
    show UnexpectedVoidExpRet = "Void type expression used with non-void return"
    show VagueBlockT = "Ambigous return type in block statement"

data RTError
    = DivideByZeroEx
    | BadRefException
    | MissingRetStmt FunId
    | BreakLoop
    | ContinueLoop

instance Show RTError where
    show DivideByZeroEx = "Runtime Error. Divide by zero"
    show BadRefException = "Runtime Error. Attempt to pass an rvalue by reference"
    show (MissingRetStmt f) = "Runtime Error. No return from non-void declared function " ++ showId f