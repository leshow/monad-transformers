{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( runProgram,
    parseCLI
    ) where

import qualified Control.Exception    as E
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Bifunctor       as BF
import qualified Data.Bool            as B
import qualified Data.Char            as C
import           Options.Applicative

data AppError = IOError E.IOException

type AppConfig = MonadReader Options

newtype App a = App {
    runApp :: ReaderT Options (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, AppConfig, MonadIO, MonadError AppError)
-- remember AppConfig here is MonadReader Options

data Options = Options
    { oCapitalize :: Bool
    , oExcited    :: Bool
    , oStdIn      :: Bool
    , oFileToRead :: Maybe String
    }

getSource :: App String
getSource = do
    isStdIn <- asks oStdIn
    B.bool loadContents (liftIO getContents) isStdIn

-- because we specify AppConfig m constraint, we have access to oCapitalize from Options
handleCapitalization :: AppConfig m => String -> m String
handleCapitalization s = fmap (B.bool s (map C.toUpper s)) (asks oCapitalize)

-- we need to use fmap because we are in a monad context, so we
-- partially apply the first 2 arguments to B.bool (a -> a -> Bool -> a)
-- then use fmap to apply the 3rd argument, our conditional (m Bool, where m is AppConfig)
handleExcitedness :: AppConfig m => String -> m String
handleExcitedness str = fmap (B.bool str ("ZOMG " ++ str)) (asks oExcited)

-- loadContents :: Options -> IO (Either String String)
-- loadContents o = maybe defaultResponse readFileFromOptions $ oFileToRead o
--     where
--         readFileFromOptions f = BF.first show <$> safeReadFile f
--         defaultResponse = return $ Right "This is fun!"


-- The maybe function takes a default value, a function, and a Maybe value.
-- If the Maybe value is Nothing, the function returns the default value.
-- Otherwise, it applies the function to the value inside the Just and returns the result.
loadContents :: App String
loadContents = do
    file <- asks oFileToRead
    maybe defaultResponse readFileFromOptions file
        where
            readFileFromOptions :: (AppConfig m, MonadIO m, MonadError AppError m) => FilePath -> m String
            readFileFromOptions f = do
                res <- fmap (BF.first IOError) (liftIO (safeReadFile f))
                either throwError return res
            defaultResponse = return "This is fun!"

-- cli parsing
parseCLI :: IO Options
parseCLI = execParser (withInfo parseOptions "File Fun")
  where
    withInfo opts h = info (helper <*> opts) $ header h

parseOptions :: Parser Options
parseOptions = Options
    <$> switch
        (long "capitalize"
        <> help "Capitalize the src file")
    <*> switch
        (long "excited"
        <> help "Make the file real excited")
    <*> switch
        (long "stdin"
        <> help "redir from stdin")
    <*> optional
        (strOption $
            long "file"
            <> help "File to parse")

-- safer reading of files

safeReadFile :: FilePath -> IO (Either E.IOException String)
safeReadFile = E.try . readFile

{- MAIN
The right half of bind, runExceptT (runReaderT (runApp run) o), runs our App in the reverse
order it was declared, inside-out.

    runApp :: ReaderT Options (ExceptT AppError IO) a

First, we have to process ExceptT, then move outward to ReaderT. The nesting is also
important here, since we want to handle failure; the result type of the right-hand side is:

    IO (Either AppError ())
The left-hand side (either renderError return) handles the success case by returning the value
(in this situation, void) or by calling renderError, applying our AppError.
-}
runProgram :: Options -> IO ()
runProgram o = do
    x <- runExceptT (runReaderT (runApp run) o) ---runExceptT and runReaderT access the wrapped representation of our newtype App
    either renderError return x

run :: App ()
run = liftIO . putStr
    =<< handleExcitedness
    =<< handleCapitalization
    =<< getSource

renderError :: AppError -> IO ()
renderError (IOError e) = do
    putStrLn "There was an error"
    putStrLn (" " ++ show e)
