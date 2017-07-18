{-# LANGUAGE OverloadedStrings #-}

module Lash.Builtin where

import Lash

import System.Exit
import System.IO
import System.Directory

type Builtin = Shell -> [String] -> (Handle, Handle, Handle) -> IO Shell

getBuiltin :: String -> Maybe Builtin
getBuiltin b =
  case b of
    "true" -> Just true
    "false" -> Just false
    "echo" -> Just echo
    "cd" -> Just cd
    _ -> Nothing

true :: Builtin
true shell _ _ = return shell {lastExitCode = ExitSuccess}

false :: Builtin
false shell _ _ = return shell {lastExitCode = ExitFailure 1}

echo :: Builtin
echo shell args (_, out, _) =
  hPutStrLn out (unwords args) >> return shell {lastExitCode = ExitSuccess}

cd :: Builtin
cd shell [] hnds = do
  home <- getHomeDirectory
  cd shell [home] hnds
cd shell (dir:_) (_, _, err) = do
    exists <- doesDirectoryExist dir
    _ <- if exists
          then setCurrentDirectory dir
          else hPutStrLn err ("cd: no such file or directory: " ++ dir)
    return shell
