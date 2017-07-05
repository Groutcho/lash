{-# LANGUAGE OverloadedStrings #-}
module Lash.ASTSpec where

import Lash.AST

import Data.Text
import Test.Hspec
import Test.QuickCheck

import Prelude hiding (Word)

instance Arbitrary Word where
    arbitrary = return (Unquoted "hello")

-- instance Arbitrary Word where
--   arbitrary = do
--     s <- arbitrary :: Gen String
--     let s' = trimWs (filter (\c -> c `elem` "\t\n\r") s)

--     -- Generating and shrinking empty words should ideally
--     -- be possible, but the parser will barf since most of
--     -- the time words cannot be empty. We should try to test
--     -- empty words using a different method.
--     let s'' = if null s' then "x" else s'
--     oneof [ return $ Quoted (concatMap escapeQuote s'')
--           , return $ Unquoted (concatMap escape s'')
--           ]