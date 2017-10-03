module Explicit.Example where

import Control.Monad.Writer.Lazy

import Explicit.Usage

run :: [String] -> Writer [String] ()
run args = do
  tell [ "Hello!"
       , "This is an example program for testing of Explicit module."
       , "The arguments are processed as filenames"
       ]
  case args of
    ("-h" : _) -> help
    _ -> tell args

help :: Writer [String] ()
help = do
  tell [ "The example program."
       , "This message is displayed to indicate usage."
       ]
  tell [display usage]

usage :: Language Alphabet
usage = lit "example" #- (lit "-h" |- Kleene (meta "filepath"))
