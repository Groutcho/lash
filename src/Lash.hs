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
  { variables :: [Variable]
  , inputStreams :: [Handle]
  , outputStreams :: [Handle]
  , errorStreams :: [Handle]
  , pendingActions :: [IO ()]
  , lastExitCode :: ExitCode
  }

setVar :: Name -> Word -> Shell -> Shell
setVar name value shell =
  shell { variables = ( (Variable name value):vars) }
  where vars = filter (\(Variable n _) -> n /= name) (variables shell)

getVar :: Name -> Shell -> Maybe Variable
getVar n s = let result = filter (\(Variable n _) -> n == n) (variables s) in
             case result of
               [] -> Nothing
               (x:_) -> Just x

pushAction :: IO () -> Shell -> Shell
pushAction c s = let cs = pendingActions s in s { pendingActions = (c:cs) }

pushInputStream :: Handle -> Shell -> Shell
pushInputStream c s = s { inputStreams = (c:inputStreams s) }

popInputStream :: Shell -> (Handle, Shell)
popInputStream s = let xs = inputStreams s in (last xs, s { inputStreams = (init xs) })

pushOutputStream :: Handle -> Shell -> Shell
pushOutputStream c s = s { outputStreams = (c:outputStreams s) }

popOutputStream :: Shell -> (Handle, Shell)
popOutputStream s = let xs = outputStreams s in (last xs, s { outputStreams = (init xs) })

pushErrorStream :: Handle -> Shell -> Shell
pushErrorStream c s = s { errorStreams = (c:errorStreams s) }

popErrorStream :: Shell -> (Handle, Shell)
popErrorStream s = let xs = errorStreams s in (last xs, s { errorStreams = (init xs) })

mkShell :: Shell
mkShell = Shell { variables = []
                , pendingActions = []
                , inputStreams = []
                , outputStreams = []
                , errorStreams = []
                , lastExitCode = ExitSuccess }

instance Show Shell where
    show s = "\n============ SHELL ============\n" ++ intercalate "\n"
             [ "variables:..." ++ (intercalate "\n" ( (show $ length $ variables s):(map (\x -> "  * " ++ show x) (variables s))))
             , "$?..........." ++ show (toNum $ lastExitCode s)
             ]

substitute :: Word -> IO Word
substitute w = return w

toNum :: ExitCode -> Int
toNum ExitSuccess = 0
toNum (ExitFailure n) = n