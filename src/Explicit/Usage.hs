{-# LANGUAGE GADTs #-}

module Explicit.Usage where

import Data.Monoid

data Language a where
  Symbol :: Monoid m => m -> Language m
  Kleene :: Monoid m => Language m -> Language m
  Positive :: Monoid m => Language m -> Language m

toString :: Language String -> String
toString (Symbol m) = m
toString (Kleene l) = toString l <> "*"
toString (Positive l) = toString l <> "+"

toUsageString :: Language String -> String
toUsageString (Symbol m) = show m
toUsageString (Kleene l) = "[" <> toUsageString l <> "...]"
toUsageString (Positive l) = toUsageString l <> "..."
