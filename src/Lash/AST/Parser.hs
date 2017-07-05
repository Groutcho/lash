{-# LANGUAGE OverloadedStrings #-}
module Lash.AST.Parser where

import Lash.AST

import           Data.Maybe     (catMaybes)
import           Prelude hiding (Word)
import qualified Data.Text as T
import           Text.Parsec
import           Text.Parsec.Text

digits :: String
digits = ['0'..'9']

lowercaseLetters :: String
lowercaseLetters = ['a'..'z']

uppercaseLetters :: String
uppercaseLetters = ['A'..'Z']

letters :: String
letters = lowercaseLetters ++ uppercaseLetters

-- data IOFileMode = IOClobber
--                 | IOFrom
--                 | IOTo
--                 | IOReadWrite
--                 | IOAppend
--                 deriving (Eq, Ord, Show)

ioFile :: GenParser st IOFile
ioFile = IOFile <$> ioFileMode <*> word

ioFileMode :: GenParser st IOFileMode
ioFileMode = do
    c <- oneOf "><"
    c' <- optionMaybe $ oneOf "><|&"
    let xs = catMaybes [Just c, c']
    case xs of
        ">" -> return IOTo
        "<" -> return IOFrom
        ">|" -> return IOClobber
        ">>" -> return IOAppend
        "<>" -> return IOReadWrite
        "<&" -> return IOFromBoth
        ">&" -> return IOToBoth

assignment :: GenParser st Assignment
assignment = do
    spaces
    n <- name
    _ <- char '='
    ow <- optionMaybe word
    case ow of
        Just w -> return (Assignment n w)
        Nothing -> return (Assignment n EmptyWord)

name :: GenParser st Name
name = do
    let initChars = ('_':letters)
    x <- oneOf initChars
    xs <- many $ oneOf (digits ++ initChars)
    return $ Name (T.pack (x:xs))

word :: GenParser st Word
word = spaces >> (quoted <|> unquoted)

quoted :: GenParser st Word
quoted = do
    c <- oneOf ['"', '\'']
    s <- manyTill quotedWordChar (char c)
    case s of
        [] -> return EmptyWord
        _  -> return $ Quoted (T.pack s)

unquoted :: GenParser st Word
unquoted = many1 wordChar >>= \s -> return $ Unquoted (T.pack s)

quotedWordChar :: GenParser st Char
quotedWordChar = do
    c <- noneOf "\t\r"
    case c of
        '\\' -> anyChar
        _ -> return c

wordChar :: GenParser st Char
wordChar = do
    c <- noneOf "; \t\r\n\"'"
    case c of
        '\\' -> anyChar
        _ -> return c
