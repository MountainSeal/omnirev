{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}
-- automatically generated by BNF Converter
module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)
import Options.Applicative
import Data.Semigroup ((<>))

import Omnirev.LexOmnirev
import Omnirev.ParOmnirev
import Omnirev.PrintOmnirev
import Omnirev.AbsOmnirev
import Omnirev.CheckOmnirev
import Omnirev.EvalOmnirev




import Omnirev.ErrM
import Control.Monad.Writer.Strict
import qualified Data.Map as M
import Data.List (intercalate)

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runParse :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
runParse v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree
                          exitSuccess

runCompile :: Verbosity -> ParseFun Program -> FilePath -> Bool -> Bool -> Bool -> IO ()
runCompile v p f cflg eflg lflg = do
  putStrLn f
  src <- readFile f
  let ts = myLLexer src
  case p ts of
    Bad s -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      putStrV v $ show ts
      putStrLn  s
      exitFailure
    Ok tree -> do
      if lflg then do
        putStrLn "\nParse Successful!"
        showTree v tree
      else pure ()
      if not cflg then exitSuccess
      else case check tree of
        Bad s -> do
          putStrLn s
          putStrLn "\nType Check         Failed...\n"
          exitFailure
        Ok (env, clog) -> do
          if lflg then do
            putStrLn "\nCheck Successful!"
            putStrLn $ unlines clog
          else pure ()
          if not eflg then exitSuccess
          else case eval env of
            Bad err -> do
              putStrLn err
              putStrLn "\nEval               Failed...\n"
              exitFailure
            Ok (res,elog) -> do
              if lflg then do
                putStrLn "\nEval Successful!"
                putStrLn $ unlines elog
              else pure ()
              putStrLn $ unlines $ map (\(i,tm) -> printTree i ++ " = " ++ printTree tm) res
              exitSuccess

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = pas =<< execParser opts
  where
    opts = info (args <**> helper) -- 常にヘルプ文出す場合はhelper使う
      (  fullDesc
      <> header "omnirev - interpreter for omnirev"
      )

pas :: Args -> IO ()
pas Version = exitFailure
pas (FileInput path cflg eflg lflg oPath) = runCompile 2 pProgram path cflg eflg lflg
pas StdInput = getContents >>= runParse 2 pProgram

data Args
  = Version
  | StdInput
  | FileInput
    { filePath :: String
    , fcheck :: Bool
    , feval :: Bool
    , flog :: Bool
    , output :: Maybe FilePath
    } deriving (Read, Show)

fileInput :: Parser Args
fileInput = do
  filePath <- strArgument
    ( help "Program file"
   <> metavar "<filepath>" )
  fcheck <- switch
    ( help "Check type of files"
   <> long "check"
   <> short 'c' )
  feval <- switch
    ( help "Evaluate program of files"
   <> long "eval"
   <> short 'e' )
  flog <- switch
    ( help "Print log"
   <> long "log"
   <> short 'l' )
  output <- optional $ strOption
    ( help "Choose output directory (default current directory)"
   <> long "output"
   <> short 'o'
   <> metavar "<path>" )
  pure FileInput {..}

stdInput :: Parser Args
stdInput =
  pure StdInput

version :: Parser Args
version = do
  infoOption "0.4.0"
    ( help "Show version"
   <> long "version"
   <> short 'v' )
  pure Version

args :: Parser Args
args = fileInput <|> stdInput <|> version