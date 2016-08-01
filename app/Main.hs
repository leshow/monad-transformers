module Main where

import           Lib

main :: IO ()
main = do
    cli <- parseCLI
    runProgram cli
