{-# LANGUAGE GADTs #-}

module Explicit.Usage where

data Language a where
  Symbol :: Monoid m => m -> Language m
  Kleene :: Monoid m => Language m -> Language m

toString :: Language String -> String
toString (Symbol m) = m
toString (Kleene l) = toString l `mappend` "*"
