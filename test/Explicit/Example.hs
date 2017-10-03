module Explicit.Example where

import Control.Monad.Writer.Lazy

run :: [String] -> Writer [String] ()
run args = do
  tell [ "Hello!"
       , "This is an example program for testing of Explicit module."
       , "The arguments are processed as filenames"
       ]
  tell args
