module Main where

import Data.Tree
import Control.Monad.Reader

import Grammar
import LL1Parser
import Tests (unwrapExcept)
import MarkedString

----grammars

-- markerRepresentation :: Marker -> Char
-- markerRepresentation (NTBracket True) = '_'
-- markerRepresentation (NTBracket False) = '^'
-- markerRepresentation RuleDivider = '$'
-- markerRepresentation Arrow = '~'
-- markerRepresentation (WildCardBracket True) = '!'
-- markerRepresentation (WildCardBracket False) = '@'
-- markerRepresentation WildCardSeperator = '&'


testG = mkGrammar "ST" [('S', "T"), ('S', "(S+T)"), ('T', "a")]
testI = unwrapExcept $ mkParseInfo testG

gblub = stringToGrammar "_S_~_T_$_S_~(^A^_S_^A^+^A^_T_^A^)$_T_~a$^A^~$^A^~ "
iblub = unwrapExcept $ mkParseInfo gblub
-- a
-- [0,2]
-- (a+a)
-- [1,0,2,2]
-- ( a +a)
-- [1,0,2,2]

-- gblub = stringToGrammar "_S_~_T_$_S_~(_A__S__A_+_A__T__A_)$_T_~a$_A_~$_A_~ "
-- iblub = unwrapExcept $ mkParseInfo gblub
-- a
-- [0,2]
-- (a+a)
-- [1,3,0,2,3,3,2,3]

gbli = stringToGrammar "_S_~^T^a$^T^~ $^T^~"
ibli = unwrapExcept $ mkParseInfo gbli

g3 = stringToGrammar "_S_~\"_T_\"$_T_~@\"@_T_$_T_~"
i3 = unwrapExcept $ mkParseInfo g3

main = do
  s <- getLine
  let rules = unwrapExcept $ (runReaderT (parse s) i3)
  print rules
  let tree = mkParseTree rules
  --print tree
  putStr $ drawTree $ fmap show tree
  print $ flatten tree
  --print ""

