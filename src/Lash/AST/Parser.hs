{-# LANGUAGE OverloadedStrings #-}

module Lash.AST.Parser where

import Lash.AST

import Data.Either (rights, lefts)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Text as T
import Prelude hiding (Word)
import qualified Text.Parsec as P
import Text.Parsec hiding (parse)
import Text.Parsec.Text

digits :: String
digits = ['0' .. '9']

lowercaseLetters :: String
lowercaseLetters = ['a' .. 'z']

uppercaseLetters :: String
uppercaseLetters = ['A' .. 'Z']

letters :: String
letters = lowercaseLetters ++ uppercaseLetters

ioFile :: GenParser st IOFile
ioFile = IOFile <$> optionMaybe fd <*> ioFileMode <*> word

parseUnsafe :: T.Text -> Sequence
parseUnsafe s = let (Right x) = parse s in x

parse :: T.Text -> Either ParseError Sequence
parse = P.parse condSeq ""

commandList :: GenParser st CommandList
commandList = CommandList <$> sepBy condSeq separator

separator :: GenParser st ()
separator = spaces >> (char '&' <|> char ';') >> return ()

condSeq :: GenParser st Sequence
condSeq =
  choice
    [ try $ CondSeq <$> pipeline <*> condOp <*> condSeq
    , try $ CondEnd <$> pipeline]

condSeqElmt :: GenParser st (Pipeline, Maybe ConditionalOperator)
condSeqElmt = do
  p <- pipeline
  c <- optionMaybe condOp
  return (p, c)

condOp :: GenParser st ConditionalOperator
condOp = spaces >> (orIf <|> andIf)

orIf :: GenParser st ConditionalOperator
orIf = string "||" >> return OrIf

andIf :: GenParser st ConditionalOperator
andIf = string "&&" >> return AndIf

pipeline :: GenParser st Pipeline
pipeline = do
  spaces
  bang <- optionMaybe (char '!')
  spaces
  cmds <- sepBy command pipeSymbol
  return (Pipeline (isJust bang) cmds)

pipeSymbol :: GenParser st ()
pipeSymbol = spaces >> char '|' >> spaces >> return ()

command :: GenParser st Command
command = spaces >> simpleCommand

simpleCommand :: GenParser st Command
simpleCommand = do
  ioa <- many (try ioFileOrAssignment)
  cmdName <- optionMaybe word
  iow <- many ioFileOrWord
  return $
    SimpleCommand (rights ioa) cmdName (rights iow) (lefts iow ++ lefts ioa)

ioFileOrAssignment :: GenParser st (Either IOFile Assignment)
ioFileOrAssignment = (Right <$> assignment) <|> (Left <$> ioFile)

ioFileOrWord :: GenParser st (Either IOFile Word)
ioFileOrWord =  (Left <$> ioFile) <|> (Right <$> word)

fd :: GenParser st Int
fd = do
  d <- many1 $ oneOf digits
  return (read d :: Int)

ioFileMode :: GenParser st IOFileMode
ioFileMode = do
  c <- oneOf "><"
  c' <- optionMaybe $ oneOf "><|&"
  let xs = catMaybes [Just c, c']
  spaces
  case xs of
    ">" -> return IOTo
    "<" -> return IOFrom
    ">|" -> return IOClobber
    ">>" -> return IOAppend
    "<>" -> return IOReadWrite
    "<&" -> return IOFromBoth
    ">&" -> return IOToBoth
    _ -> error "just to appease the type checker"

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
  let initChars = ('_' : letters)
  x <- oneOf initChars
  xs <- many $ oneOf (digits ++ initChars)
  return $ Name (T.pack (x : xs))

word :: GenParser st Word
word = do
  spaces
  w <- (quoted <|> unquoted)
  spaces
  return w

quoted :: GenParser st Word
quoted = do
  c <- oneOf ['"', '\'']
  s <- manyTill quotedWordChar (char c)
  case s of
    [] -> return EmptyWord
    _ ->
      if c == '\''
        then return $ SingleQuoted (T.pack s)
        else return $ Quoted (T.pack s)

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
  c <- noneOf "><&|; \t\r\n\"'"
  case c of
    '\\' -> anyChar
    _ -> return c
