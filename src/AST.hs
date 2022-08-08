{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
module AST where

import Types
import Env

data Expr a where
    Abs :: (Expr a -> Expr b) -> Expr (a -> b)
    Apply :: Expr (a -> b) -> Expr a -> Expr b
    Let :: Identifier -> Expr a -> Expr b
    If :: Expr Bool -> Expr a -> Expr a -> Expr a
    Binop ::  (a -> b -> c) -> Expr a -> Expr b -> Expr c
    Unop :: (a -> b) -> Expr a -> Expr b
    Lift :: a -> Type a -> Expr (Type a)
    Var :: Identifier -> Type a -> Expr a

type TypedExpr a = Expr (Type a)





