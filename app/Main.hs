module Main where

import Data.Tree
import Control.Monad.Reader

import Grammar
import LRparser
import Tests

----grammars

testG = mkGrammar "ST" [('S', "T"), ('S', "(S+T)"), ('T', "a")]

--testT = unwrapExcept $ mkTable testG

testI = unwrapExcept $ mkParseInfo testG

g = Grammar 2 ["\"" , "a"] [ ( 0 , Rule ( 0, [TChar "\"" , NTChar 1, TChar "\""]) ) , (1, Rule ( 1, [BL ['\"'], NTChar 1]))  , (2, Rule ( 1, [])) ]

--t = unwrapExcept $ mkTable g

i = unwrapExcept $ mkParseInfo g

main = do
  s <- getLine
  let rules = unwrapExcept $ (runReaderT (parse s) i)
  print rules
  let tree = mkParseTree rules
  --print tree
  putStr $ drawTree $ fmap show tree
  print $ flatten tree
  --print ""

