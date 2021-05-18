module Main where

import Control.Monad.Except
import Control.Monad.Trans.Except
import System.Environment
import System.Exit
import System.IO

import AbsGrammar
import ErrM
import ParGrammar

import Interpreter
import TypeCheck
import Types


main :: IO ()
main = do
    args <- getArgs
    case args of 
        [] -> getContents >>= run
        fs -> mapM_ runFile fs

runFile :: String -> IO ()
runFile fName = readFile fName >>= run

run :: String -> IO ()
run code = 
    case pProgram (myLexer code) of
        (Bad e) -> exitWithErrorMsg $ "Parse error. " ++ e
        (Ok t) -> do
            tcResult <- runExceptT $ typeCheck t
            case tcResult of 
                Left e -> exitWithErrorMsg $ "Typecheck failed. " ++ show e
                Right _ -> do
                    progResult <- runExceptT $ interpret t
                    case progResult of 
                        Left e -> exitWithErrorMsg $ show e
                        Right r -> return ()
            return ()

exitWithErrorMsg :: String -> IO ()
exitWithErrorMsg msg = do
    hPutStrLn stderr msg
    exitFailure

