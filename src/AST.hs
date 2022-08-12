module AST where
import Types

data TypeParam = TypeParam Id Type

data ExprF a = Variable Id
    | Parametrized Id [Types]
    | Val Id a
    | Def Id [Id] [TypeParam] a
    | Lambda TypeParam a
    | Apply a a
    | If a a a
    | Num Int
    | Bool Bool
    | Char Char
    | Unit
    | Seq a a
    | Typed Type a

newtype Expr = Expr (ExprF Expr)
data TExpr = TExpr (ExprF TExpr) Type

typecheck :: Expr -> Env Type -> TExpr





