module AST where

type Identifier = String

type Environment = Map.Map Identifier Expr

data Expr = 
    | Abs Identifier Expr
    | Apply Expr Expr
    | Let Identifier Expr Expr
    | If Expr Expr Expr
    | Binop Op Expr Expr
    | Unop Op Expr
    | Const Value
    | Var Identifier

data Value = Num Integer | Bool Boolean

data Op = 
    Plus | Minus | Mult | Div |
    Not | And | Or

