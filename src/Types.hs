{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

module Types where
import Control.Monad
import Data.Either
import Data.Bifunctor

type Identifier = String
---------------------------------------------------------------
data Type t where
    Unit :: Type () 
    RInt :: Type Int
    RChar :: Type Char
    RBool :: Type Bool
    RList :: Type a -> Type [a]
    (:*:) :: Type a -> Type b -> Type (a, b)
    (:+:) :: Type a -> Type b -> Type (Either a b)
    (:->) :: Type a -> Type b -> Type (a -> b)
    Tagged :: Identifier -> Type a -> Type a

instance forall t . Show (Type t) where
    show Unit = "()"
    show RInt = "Int"
    show RChar = "Char"
    show RBool = "Bool"
    show (RList t) = "List of " ++ show t
    show (t :+: u) = show t ++ " union " ++ show u
    show (t :*: u) = "(" ++ show t ++ "," ++ show u ++ ")"
    show (t :-> u) = show t ++ " -> " ++ show u
    show (Tagged id t) = id ++ ": " ++ show t

instance forall t . Eq (Type t) where
    Unit == Unit = True
    RInt == RInt = True
    RChar == RChar = True
    RBool == RBool = True
    (RList t) == (RList u) = t == u
    (a :+: t) == (b :+: u) = a == b && t == u
    (a :*: t) == (b :*: u) = a == b && t == u
    (a :-> t) == (b :-> u) = a == b && t == u
    (Tagged a t) == (Tagged b u) = a == b && t == u

rString :: StringT
rString = RList RChar

type IntT = Type Int
type BoolT = Type Bool
type CharT = Type Char
type StringT = Type [Char]
type ListT a = Type [a]
---------------------------------------------------------------
data Untyped = forall t . Untyped (Type t) t

instance Show Untyped where
    show (Untyped t b) = "Variable of " ++ show t

data Rep = forall t . Rep (Type t)

instance Show Rep where
    show (Rep t) = show t

tequal :: Type t -> Type u -> Maybe (t -> u)
tequal RBool RBool = return id
tequal RChar RChar = return id
tequal RInt RInt = return id
tequal (RList t) (RList u) = liftM map (tequal t u)
tequal (a :*: b) (t :*: u) = 
    liftM2 (\f g p -> (f (fst p), g (snd p))) (tequal a t) (tequal b u)
tequal (a :+: b) (t :+: u) = do
    aToT <- tequal a t
    bToU <- tequal b u
    Just $ bimap aToT bToU
tequal (a :-> b) (t :-> u) = do
    tToA <- tequal t a
    bToU <- tequal b u
    Just $ \f x -> bToU . f . tToA $ x
tequal (Tagged idA a) (Tagged idB b) =
    if idA /= idB then Nothing else tequal a b
tequal _ _ = Nothing

cast :: Untyped -> Type t -> Maybe t
cast (Untyped t a) u = fmap (\f -> f a) (tequal t u)