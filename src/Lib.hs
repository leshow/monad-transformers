{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( runStuff
    ) where

import qualified Control.Exception as E
import qualified Data.Bifunctor as BF
import qualified Data.Bool as B
import qualified Data.Char as C
import Control.Monad.Reader
import Options.Applicative

data Options = Options
    { oCapitalize :: Bool
    , oExcited :: Bool
    , oStdIn :: Bool
    , oFileToRead :: Maybe String
    }

getSource :: Options -> IO String
getSource o = if oStdIn o then getContents else either id id <$> loadContents o

handleCapitalization :: Options -> String -> String
handleCapitalization o str = if oCapitalize o then map C.toUpper str else str

handleExcitedness :: Options -> String -> String
handleExcitedness o str = if oExcited o then "ZOMG " ++ str else str

loadContents :: Options -> IO (Either String String)
loadContents o = maybe defaultResponse readFileFromOptions $ oFileToRead o
    where
        readFileFromOptions f = BF.first show <$> safeReadFile f
        defaultResponse = return $ Right "This is fun!"

-- cli parsing
parseCLI :: IO Options
parseCLI = execParser (withInfo parseOptions "File Fun")
  where
    withInfo opts h = info (helper <*> opts) $ header h

parseOptions :: Parser Options
parseOptions = Options
    <$> (switch $ long "capitalize")
    <*> (switch $ long "excited")
    <*> (switch $ long "stdin")
    <*> (optional $ strOption $ long "file")

-- safer reading of files

safeReadFile :: FilePath -> IO (Either E.IOException String)
safeReadFile = E.try . readFile

-- main
runProgram :: Options -> IO ()
runProgram o =
    putStr =<< (handleExcitedness o . handleCapitalization o <$> getSource o)

runStuff :: IO ()
runStuff = runProgram =<< parseCLI
