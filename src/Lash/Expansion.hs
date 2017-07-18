{-# LANGUAGE OverloadedStrings #-}
module Lash.Expansion
  ( expand
  , expand'
  ) where

import Lash
import Lash.AST

import Control.Exception.Base
import Data.Either
import Data.List.Split
import qualified Data.Text as T
import Data.Maybe
import System.Directory
import System.IO
import System.Process
import System.FilePath.Posix
import System.FilePath.Glob
import System.Posix.User
import Text.Parsec (parse)

import Prelude hiding (Word)

expand :: Word -> Shell -> IO (Word, Shell)
expand w0 shell0 = do
  w1 <- expandTilde w0 shell0
  return (w1, shell0)

expand' :: [Word] -> Shell -> IO ([Word], Shell)
expand' [] shell = return ([], shell)
expand' (x:xs) shell = do
  (x', shell') <- expand x shell
  (xs', shell2) <- expand' xs shell'
  return ((x':xs'), shell2)

-- expand :: Expansion
-- expand w0 shell = do
--   (w1, shell2) <- expandTilde w0 shell
--   case w1 of
--     Left err -> return $ Left err
--     Right w -> do
--       w2 <- expandParameters w
--       case w2 of
--         Left err -> return $ Left err
--         Right w' -> substituteCommand w'

expandTilde :: Word -> Shell -> IO Word
expandTilde EmptyWord shell = return EmptyWord
expandTilde w@(Quoted _) shell = return w
expandTilde w@(SingleQuoted _) shell = return w
expandTilde w@(Unquoted s) shell =
  case (T.unpack s) of
    ('\\':_) -> return w
    ('"':_) -> return w
    "~" -> substituteHome Nothing ""
    ('~':'/':path) -> substituteHome Nothing path
    ('~':xs) -> do
      let parts = split (oneOf "/") xs
      case parts of
        [user] -> substituteHome (Just user) ""
        (user:"/":path) -> substituteHome (Just user) (concat path)
    _ -> return w

substituteHome :: Maybe String -> String -> IO Word
substituteHome Nothing path = do
  home <- getHomeDirectory
  return $ Quoted $ T.pack $ home </> path
substituteHome (Just user) path = do
  attempt <- try (getUserEntryForName user) :: IO (Either IOError UserEntry)
  case attempt of
    Right entry -> return $ Quoted $ T.pack ((homeDirectory entry) </> path)
    Left _ -> do
      hPutStrLn stderr ("lash: no such user or named directory: " ++ user)
      return EmptyWord

expandGlob :: Word -> Shell -> IO [Word]
expandGlob (Unquoted s) shell = do
  xs <- glob (T.unpack s)
  return $ Unquoted <$> map (T.pack . takeFileName) xs

-- expandParameters :: Expansion Word
-- expandParameters w = do
--   let p = parse parameters "" (getWordValue w)
--   ctx <- get
--   case p of
--     Right params -> do
--       let (errs, successes) =
--             partitionEithers $ map (expandParameter (variables ctx)) params
--       if null errs
--         then return $ Right $ Quoted (concat successes)
--         else return $ Left $ head errs
--     Left _ -> return $ Left (Error "xsh: parse error")

-- expandParameter :: [Variable] -> Parameter -> Either ExpansionError String
-- expandParameter vars (Parameter (Name s) fmt) =
--   case fmt of
--     Plain -> Right $ getVar s vars
--     UseDefaultValue _ def -> Right $ getVarDefault s vars (getWordValue def)
--     AlternativeValue _ alt ->
--       let l = lookup s vars
--       in if isNothing l
--            then Right ""
--            else Right (getWordValue alt)
--     ErrorIfNullOrUnset _ msg ->
--       let l = lookup s vars
--       in if isNothing l
--            then Left $
--                 Error $
--                 maybe
--                   ("xsh: " ++ s ++ ": parameter not set")
--                   (\m -> "xsh: " ++ s ++ ": " ++ (getWordValue m))
--                   msg
--            else Right (fromJust l)
--     StringLength -> Right $ (show . length) (getVarDefault s vars "")

-- substituteCommand :: Expansion Word
-- substituteCommand w@(Quoted s) = do
--   let resut = parse commandSubstitutions "" s
--   case resut of
--     Left _ -> return $ Left $ Error ("xsh: parse error near `" ++ s ++ "'")
--     Right cmds -> do
--       outputs <- liftIO $ mapM substitute cmds
--       return $ Right $ Quoted (unwords outputs)

-- substitute :: CommandSubstitution -> IO String
-- substitute (NoSubstitution s) = return s
-- substitute (CommandSubstitution name args) = do
--   output <- readProcess name args ""
--   return output