{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

module Env where
import Types
import Control.Arrow

data Entry = Entry {
    getId :: Identifier,
    getDyn :: Untyped
} deriving Show

type Env = [Entry]

lookUp :: Identifier -> Env -> Maybe Untyped
lookUp id = filter (getId >>> (== id)) >>> safeHead >>> fmap getDyn
    where 
        safeHead [] = Nothing
        safeHead (e:es) = Just e
