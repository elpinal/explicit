{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Explicit.Usage where

import Data.Monoid

data Language a where
  Symbol :: Monoid m => m -> Language m
  Kleene :: Monoid m => Language m -> Language m
  Positive :: Monoid m => Language m -> Language m
  Union :: Monoid m => Language m -> Language m -> Language m
  Concat :: Monoid m => Language m -> Language m -> Language m
  Option :: Monoid m => Language m -> Language m

deriving instance Show a => Show (Language a)

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
format (Kleene l) = Meta "[" <> parens (isBinOp l) (format l) <> Meta "...]"
format (Positive l) = parens (isBinOp l) (format l) <> Meta "..."
format (Union l m) = format l <> Meta " | " <> format m
format (Concat l m) = parens (isUnion l) (format l) <> Meta " " <> parens (isUnion m) (format m)
format (Option l) = Meta "[" <> format l <> Meta "]"

parens :: Bool -> Alphabet -> Alphabet
parens b x = if b then Meta "(" <> x <> Meta ")" else x

isBinOp :: Language a -> Bool
isBinOp (Union _ _) = True
isBinOp (Concat _ _) = True
isBinOp _ = False

isUnion :: Language a -> Bool
isUnion (Union _ _) = True
isUnion _ = False

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

instance Display Alphabet where
  display (Literal s) = show s
  display (Meta s) = s

lit :: String -> Language Alphabet
lit = Symbol . Literal

meta :: String -> Language Alphabet
meta = Symbol . Meta

newtype MetaDef = MetaDef (String, Language Alphabet)
  deriving Show

instance Display MetaDef where
  display (MetaDef (lhs, rhs)) = lhs ++ " = " ++ display (format rhs)

data Usage = Usage (Language Alphabet) [MetaDef]
  deriving Show

instance Display Usage where
  display (Usage l defs) = unlines $ [display $ format l , ""] ++ map display defs

class Display a where
  display :: a -> String
