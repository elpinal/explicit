{-# LANGUAGE GADTs #-}

module Explicit.Usage where

import Data.Monoid

data Language a where
  Symbol :: Monoid m => m -> Language m
  Kleene :: Monoid m => Language m -> Language m
  Positive :: Monoid m => Language m -> Language m
  Union :: Monoid m => Language m -> Language m -> Language m
  Concat :: Monoid m => Language m -> Language m -> Language m
  Option :: Monoid m => Language m -> Language m

infixl 2 |-
(|-) :: Monoid a => Language a -> Language a -> Language a
(|-) = Union

infixl 3 #-
(#-) :: Monoid a => Language a -> Language a -> Language a
(#-) = Concat

toString :: Language String -> String
toString (Symbol m) = m
toString (Kleene l) = toString l <> "*"
toString (Positive l) = toString l <> "+"
toString (Union l m) = toString l <> "|" <> toString m
toString (Concat l m) = toString l <> toString m
toString (Option l) = toString l <> "?"

format :: Language Alphabet -> Alphabet
format (Symbol m) = m
format (Kleene l) = Meta "[" <> format l <> Meta "...]"
format (Positive l) = format l <> Meta "..."
format (Union l m) = format l <> Meta " | " <> format m
format (Concat l m) = format l <> Meta " " <> format m
format (Option l) = Meta "[" <> format l <> Meta "]"

data Alphabet =
    Literal String
  | Meta String
    deriving (Eq, Show)

instance Monoid Alphabet where
  mempty = Meta ""
  (Meta a) `mappend` (Meta b) = Meta $ a <> b
  (Meta a) `mappend` (Literal b) = Meta $ a <> show b
  (Literal a) `mappend` (Meta b) = Meta $ show a <> b
  (Literal a) `mappend` (Literal b) = Literal $ a <> b

fromAlphabet :: Alphabet -> String
fromAlphabet (Literal s) = s
fromAlphabet (Meta s) = s

lit :: String -> Language Alphabet
lit = Symbol . Literal

meta :: String -> Language Alphabet
meta = Symbol . Meta
