{-# LANGUAGE OverloadedStrings #-}

module Lash where

import Lash.AST

import Data.Monoid     ((<>))
import Data.Conduit
import Data.ByteString (ByteString)
import Data.List       (intercalate)
import Data.Text       (Text, unpack)
import Data.Streaming.Process
import System.Exit     (ExitCode(..))
import System.IO
import Prelude hiding  (Word)

data Variable = Variable { varName :: Name
                         , varValue :: Word
                         }

instance Show Variable where
  show (Variable (Name n) v) = unpack $ n <> "=" <> getValue v

data Shell = Shell
  { shVariables :: [Variable]
  , shWaiters :: [IO ExitCode]
  , shStdin :: Handle
  , shStdout :: Handle
  , shConduits :: [IO ()]
  , shLastExitCode :: ExitCode
  }

setVar :: Name -> Word -> Shell -> Shell
setVar name value shell =
  shell { shVariables = ( (Variable name value):vars) }
  where vars = filter (\(Variable n _) -> n /= name) (shVariables shell)

getVar :: Name -> Shell -> Maybe Variable
getVar n s = let result = filter (\(Variable n _) -> n == n) (shVariables s) in
             case result of
               [] -> Nothing
               (x:_) -> Just x

addWaiter :: IO ExitCode -> Shell -> Shell
addWaiter ac shell =
  let acs = shWaiters shell in
  shell { shWaiters = (ac:acs)}

clearWaiters :: Shell -> Shell
clearWaiters s = s { shWaiters = [] }

addConduit :: IO () -> Shell -> Shell
addConduit c s = let cs = shConduits s in s { shConduits = (c:cs) }

clearConduits :: Shell -> Shell
clearConduits s = s { shConduits = [] }

mkShell :: IO Shell
mkShell = do
    return Shell { shVariables = []
                 , shWaiters = []
                 , shConduits = []
                 , shStdin = stdin
                 , shStdout = stdout
                 , shLastExitCode = ExitSuccess }

instance Show Shell where
    show s = "================================\n" ++ intercalate "\n" [ "running: " ++ (show $ length $ shWaiters s)
                              , "variables:\n" ++ (intercalate "\n" (map (\x -> "  * " ++ show x) (shVariables s)))
                              , "last exit code: " ++ show (shLastExitCode s)
                              ]

substitute :: Word -> IO Word
substitute w = return w