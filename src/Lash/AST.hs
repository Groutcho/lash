module Lash.AST where

import Data.Text        (Text)
import Prelude   hiding (Word)

data Assignment = Assignment Name Word
                  deriving (Eq, Ord, Show)

type Filename = Text

data IOFile = IOFile IOFileMode Word
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
          | Unquoted Text
          | EmptyWord
          deriving (Eq, Ord, Show)

newtype Name = Name Text
               deriving (Eq, Ord, Show)