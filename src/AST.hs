module AST where

data Expr = Variable Id
    | Let TypedParam Expr Expr 
    | Lambda TypedParam Expr 
    | Apply Expr Expr 
    | If Expr Expr Expr 
    | Num Int
    | Bool Bool
    | Char Char
    | Unit
    | Seq Expr Expr 
    | Typed Expr Type 
    deriving (Show, Eq)

formatExpr :: Expr -> String
formatExpr = formatExpr' 0
    where 
        tabs 0 = ""
        tabs n = "\t" ++ tabs (n - 1)   
        formatExpr' n (Let typed e1 e2) = tabs n "let\n" ++ tabs (n + 1) ++ show typed ++ " = " ++ show e1 ++ tabs n "in\n" ++ tabs (n + 1) ++ show e2 ++ "\n"


data Printer = Printer Int String

data TypedParam = TypedParam Id Type
    deriving (Eq)
instance Show TypedParam where
    show (TypedParam id t) = id ++ " :: " ++ show t

type Id = String

data Type = VariableT Id
    | UnitT
    | IntT
    | BoolT
    | CharT
    | ListT Type
    | Type :*: Type
    | Type :+: Type
    | Type :-> Type
    | Tagged Id Type
    | Ref Type
    deriving Eq

stringT :: Type
stringT = ListT CharT

instance Show Type where
    show (VariableT id) = id
    show UnitT = "()"
    show IntT = "Int"
    show CharT = "Char"
    show BoolT = "Bool"
    show (ListT t) = "[" ++ show t ++ "]"
    show (t :+: u) = show t ++ " + " ++ show u
    show (t :*: u) = show t ++ " * " ++ show u
    show (t :-> u) = show t ++ " -> " ++ show u
    show (Tagged id t) = id ++ ": " ++ show t
    show (Ref t) = "Reference to " ++ show t




