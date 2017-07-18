{-# LANGUAGE OverloadedStrings #-}
module Lash.Interactive where

import Lash
import Lash.AST
import Lash.AST.Parser
import Lash.Instructions
import Lash.Expansion

import           Control.Monad (forever, when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.IO
import           System.Directory
import           Text.Printf (printf)
import           System.Console.ANSI
import           System.Console.Haskeline
import           System.Posix.User

repl :: IO ()
repl = do
  let shell = mkShell
  performCycle shell

prompt :: String -> IO String
prompt fmt = do
  cwd <- getCurrentDirectory
  let s = printf "\x1b[1;94m%s\x1b[1;93m λ \x1b[m" cwd
  return s

performCycle :: Shell -> IO ()
performCycle shell = do
  let ps1 = getVar' (Name "PS1") shell
  (ps1', shell') <- expand (varValue ps1) shell
  promptStr <- prompt (getValueS ps1')
  hPutStr stderr promptStr
  input <- readInput
  if T.null input
    then performCycle shell'
    else
      case parse input of
        Left err -> (putStrLn $ show err) >> performCycle shell'
        Right cmd -> do
          shell2 <- executeAll (compile cmd) shell'
          performCycle shell2

readInput :: IO T.Text
readInput = runInputT defaultSettings loop
   where
       loop :: InputT IO T.Text
       loop = do
           minput <- getInputLine ""
           case minput of
               Nothing -> return ""
               Just input -> return $ T.pack input