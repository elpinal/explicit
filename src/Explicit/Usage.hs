{-# LANGUAGE GADTs #-}

module Explicit.Usage where

import Data.Monoid

data Language a where
  Symbol :: Monoid m => m -> Language m
  Kleene :: Monoid m => Language m -> Language m
  Positive :: Monoid m => Language m -> Language m
  Union :: Monoid m => Language m -> Language m -> Language m
  Concat :: Monoid m => Language m -> Language m -> Language m

toString :: Language String -> String
toString (Symbol m) = m
toString (Kleene l) = toString l <> "*"
toString (Positive l) = toString l <> "+"
toString (Union l m) = toString l <> "|" <> toString m
toString (Concat l m) = toString l <> toString m

toUsageString :: Language String -> String
toUsageString (Symbol m) = show m
toUsageString (Kleene l) = "[" <> toUsageString l <> "...]"
toUsageString (Positive l) = toUsageString l <> "..."
toUsageString (Union l m) = toUsageString l <> "|" <> toUsageString m
toUsageString (Concat l m) = toUsageString l <> " " <> toUsageString m
