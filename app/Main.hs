{-# OPTIONS_GHC -fwarn-tabs #-}

module Main where

import Control.Monad (when)
import System.Console.GetOpt
import System.Environment
import System.IO

import Eval
import Monad
import Parser
import Syntax

data Flag = Verbose
          | NoTypeCheck
  deriving (Eq, Show)

main :: IO ()
main = do
    (flags, files) <- getArgs >>= parseOpts
    mapM_ (evalFile flags) files

parseAndEval :: [Flag] -> FilePath -> String -> EvalM [(Exp, Either StlcError Exp)]
parseAndEval flags filename s = do
    ds <- parseStlc filename s
    return $ evalDecls (NoTypeCheck `notElem` flags) ds

evalFile :: [Flag] -> String -> IO ()
evalFile flags filename = do
    s <- readFile filename
    case parseAndEval flags filename s of
      Left err -> hPutStrLn stderr ("Error: " ++ show err)
      Right res -> printResults res
  where
    printResults [] = return ()

    printResults ((e,Left err):es) = do
        when (Verbose `elem` flags) $
            putStrLn $ "Given: " ++ show e
        putStrLn ("Error: " ++ show err)
        printResults es

    printResults ((e,Right e'):es) = do
        when (Verbose `elem` flags) $
            putStrLn $ "Given: " ++ show e
        print e'
        printResults es

options :: [OptDescr Flag]
options =
 [ Option ['v'] ["verbose"]      (NoArg Verbose)      "be verbose"
 , Option ['n'] ["no-typecheck"] (NoArg NoTypeCheck)  "don't typecheck"
 ]

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv =
    case getOpt Permute options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: arith [OPTION...] files..."
