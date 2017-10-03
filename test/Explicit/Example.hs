module Explicit.Example where

import Control.Monad.Writer.Lazy

import Explicit.Usage

run :: [String] -> Writer [String] ()
run args = do
  tell runHeader
  case args of
    ("-h" : _) -> help
    _ -> tell args

runHeader :: [String]
runHeader =
  [ "Hello!"
  , "This is an example program for testing of Explicit module."
  , "The arguments are processed as filenames."
  ]

help :: Writer [String] ()
help = do
  tell helpHeader
  tell ["Usage: " ++ display usage]
  tell ["", display def]

helpHeader :: [String]
helpHeader =
  [ "The example program."
  , "This message is displayed to indicate usage."
  ]

usage :: Language Alphabet
usage = lit "example" #- (meta "flag" |- Kleene (meta "filepaths"))

def :: MetaDef
def = MetaDef ("flag", lit "-h")
