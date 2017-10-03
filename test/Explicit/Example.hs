module Explicit.Example where

import Control.Monad.Writer.Lazy
import System.Environment

import Explicit.Usage

main :: IO ()
main = fmap (execWriter . run) getArgs >>= mapM_ putStrLn

run :: [String] -> Writer [String] ()
run ("-h" : _) = help
run args = process args

processHeader :: [String]
processHeader =
  [ "Hello!"
  , "This is an example program for testing of Explicit module."
  , "The arguments are processed as filenames."
  , ""
  ]

process :: [String] -> Writer [String] ()
process [] = do
  tell processHeader
  tell ["There are no filepaths."]

process args = do
  tell processHeader
  tell ["Filepaths are:"]
  tell args

help :: Writer [String] ()
help = do
  tell helpHeader
  tell ["Usage:", "", replicate 4 ' ' ++ display usage]
  tell ["", "Name definitions:", "", replicate 4 ' ' ++ display def]

helpHeader :: [String]
helpHeader =
  [ "The example program."
  , "This message is displayed to indicate usage."
  , ""
  ]

usage :: Language Alphabet
usage = lit "example" #- (meta "flag" |- Kleene (meta "filepaths"))

def :: MetaDef
def = MetaDef ("flag", lit "-h")
