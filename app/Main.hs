module Main where

import           Control.Monad       (void)
import           Options.Applicative (Parser, argument, execParser, fullDesc,
                                      info, metavar, str)

import           Lib

parser :: Parser String
parser = argument str (metavar "<Jira domain name>")

main :: IO ()
main = do
    domain <- execParser (info parser fullDesc)
    void $ runJira domain
