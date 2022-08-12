{-# LANGUAGE TypeOperators #-}
module Types where 


type Id = String

data Type = Variable id
    | Unit
    | IntT
    | BoolT
    | CharT
    | List Type
    | Type :*: Type
    | Type :+: Type
    | Type :-> Type
    | Tagged Type
    | Ref Type
    deriving Eq

stringT :: Type
stringT = List CharT

instance Show Type where
    show Unit = "()"
    show IntT = "Int"
    show CharT = "Char"
    show BoolT = "Bool"
    show (List t) = "List of " ++ show t
    show (t :+: u) = show t ++ " union " ++ show u
    show (t :*: u) = "(" ++ show t ++ "," ++ show u ++ ")"
    show (t :-> u) = show t ++ " -> " ++ show u
    show (Tagged id t) = id ++ ": " ++ show t
    show (Ref t) = "Reference to " ++ show t