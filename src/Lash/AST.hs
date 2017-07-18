{-# LANGUAGE OverloadedStrings #-}
module Lash.AST where

import Data.Text        (Text, unpack)
import Prelude   hiding (Word)

data Assignment = Assignment Name Word
                  deriving (Eq, Ord, Show)

type Filename = Text

data CommandList
  = CommandList [Sequence] deriving (Eq, Show)

data Sequence
  = CondEnd Pipeline
  | CondSeq Pipeline
            ConditionalOperator
            Sequence
            deriving (Eq, Show)

data ConditionalOperator = AndIf | OrIf deriving (Show, Eq)

data Pipeline =
  Pipeline Bool -- inverse ExitCode
           [Command]
           deriving (Eq, Show)

data Command =
  SimpleCommand [Assignment] -- variable assignments
                (Maybe Word) -- command name
                [Word] -- arguments
                [IOFile] -- redirects
  deriving (Show, Eq)

data IOFile = IOFile (Maybe Int) IOFileMode Word
              deriving (Eq, Ord, Show)

data IOFileMode = IOClobber
                | IOFrom
                | IOFromBoth
                | IOTo
                | IOToBoth
                | IOReadWrite
                | IOAppend
                deriving (Eq, Ord, Show)

data Word = Quoted Text
          | SingleQuoted Text
          | Unquoted Text
          | EmptyWord
          deriving (Eq, Ord, Show)

getValue :: Word -> Text
getValue EmptyWord = ""
getValue (Quoted s) = s
getValue (Unquoted s) = s
getValue (SingleQuoted s) = s

getValueS :: Word -> String
getValueS = unpack . getValue

newtype Name = Name Text
               deriving (Eq, Ord, Show)