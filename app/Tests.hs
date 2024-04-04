module Tests where

import Data.Map hiding (map)

import Control.Monad.Reader
import Control.Monad.Except

import Data.Tree

import Test.QuickCheck

import Grammar
import LL1Parser
import MarkedString

data ParseStepSimple = R Int | C Char
  deriving (Eq, Show)

convParseStep :: ParseStep -> ParseStepSimple
convParseStep (ParseRule i r) = R i
convParseStep (ParseChar c) = C c

type ParseTreeSimple = Tree ParseStepSimple

-----------------functions for testing--------------------------

testTable :: Grammar -> Either TableError Table -> Bool
testTable gr exp = runExcept (mkTable gr) == exp

unwrapExcept :: Show a1 => Except a1 a2 -> a2
unwrapExcept ex = case runExcept ex of Right a -> a
                                       Left e -> error $ show e

----tests if a function returns the right values for a list of testcases
----if test is successfull, returns Right (), otherwise returns list of incorrect cases
----(as a triple of input, function output, expected value from the testcases)

testFunction :: (Eq b) => (a -> b) -> [(a,b)] -> Either [(a,b,b)] ()
testFunction f ys = Prelude.foldr helper (Right ()) ys
  where helper (x,y) ex = let y' = f x
                          in case (y == y', ex) of
                               (True, _) -> ex
                               (False, Right ()) -> Left [(x,y',y)]
                               (False, Left exs) -> Left ((x,y',y):exs)

parseForTesting :: ParseInfo -> (String, String) -> Either ParseError ([ParseStepSimple],String)
parseForTesting inf (start, s) = case runReaderT (parse start s) inf of
                                   Left e -> Left e
                                   Right (steps, rest) -> Right (map convParseStep steps, rest)

testParse :: Grammar -> [((String,String), Either ParseError ([ParseStepSimple], String))] ->
  Either [((String, String), Either ParseError ([ParseStepSimple], String), Either ParseError ([ParseStepSimple], String))] ()
testParse gr testcases
  = let inf = unwrapExcept $ mkParseInfo gr
     in testFunction (parseForTesting inf) testcases

---maybe unimportant, better test for mkTree below

mkParseTreeForTesting :: ParseInfo -> (String, String) -> Either ParseError ParseTreeSimple
mkParseTreeForTesting inf (start, s) = case runReaderT (parse start s) inf of
                                         Left e -> Left e
                                         Right (steps, rest) -> Right $ fmap convParseStep $ mkParseTree steps

testTree :: Grammar -> [((String,String), Either ParseError ParseTreeSimple)] ->
  Either [((String, String), Either ParseError ParseTreeSimple, Either ParseError ParseTreeSimple)] ()
testTree gr testtrees
  = let inf = unwrapExcept $ mkParseInfo gr
     in testFunction (mkParseTreeForTesting inf) testtrees

---test for trees with quickCheck

treeWithChildren :: Tree a -> Tree (a, Int)
treeWithChildren (Node x xs) = Node (x, length xs) (map treeWithChildren xs)

testTreeQC :: Eq a => Tree a -> Bool
testTreeQC t = mkTree (flatten ( treeWithChildren t)) == t

----------------testcases----------------------------------

-- markerRepresentation (NTBracket True) = '_'
-- markerRepresentation (NTBracket False) = '^'
-- markerRepresentation RuleDivider = '$'
-- markerRepresentation Arrow = '~'
-- markerRepresentation (WildCardBracket True) = '!'
-- markerRepresentation (WildCardBracket False) = '@'
-- markerRepresentation WildCardSeperator = '&'


g1 = stringToGrammar "_S_~_T_$_S_~(_S_+_T_)$_T_~a"
t1 = fromList [ ((0,Just (TChar "(")) , (1,Rule (0,[TChar "(",NTChar 0 True,TChar "+",NTChar 1 True,TChar ")"])))
              , ((0,Just (TChar "a")) , (0,Rule (0,[NTChar 1 True])))
              , ((1,Just (TChar "a")) , (2,Rule (1,[TChar "a"])))]
testcases1 = [ (("S", "a"), Right ([R 0,R 2], ""))
             , (("S", "(a+a)"), Right ([R 1,R 0,R 2,R 2], ""))
             , (("S", "a+a"), Right ([R 0, R 2], "+a") )
             , (("S", "(a+"), Left $ NoRule 1 "")]

g2 = stringToGrammar "_S_~aa$_S_~ab"
t2 = fromList [ ((0,Just $ TChar "aa") , (0,Rule (0,[TChar "aa"])))
              , ((0,Just $ TChar "ab") , (1,Rule (0,[TChar "ab"])))]
testcases2 = [(("S", "aa"), Right ([R 0], "")), (("S", "ab"), Right ([R 1], ""))]

g3 = stringToGrammar "_S_~\"_T_\"$_T_~@\"@_T_$_T_~"
t3 = fromList [ ((0,Just (TChar "\"")) , (0,Rule (0,[TChar "\"",NTChar 1 True,TChar "\""])))
              , ((1,Nothing) , (2,Rule (1,[])))
              , ((1,Just (WildCard False "\"")) , (1,Rule (1,[WildCard False "\"",NTChar 1 True])))]
testcases3 = [ (("S", "\"\""), Right ([R 0,R 2], ""))
             , (("S", "\"abc\""), Right ([R 0,R 1,C 'a',R 1,C 'b',R 1,C 'c',R 2], ""))
             , (("S", "\""), Left $ MissingCharacters "\"")
             , (("S", "a"), Left $ NoRule 0 "a") -- 0 [] "a" 
             , (("S", "\"a"), Left $ MissingCharacters "\"")
             , (("S", "\"a\"a"), Right ([R 0 , R 1, C 'a', R 2],  "a"))]
testtrees3 = [ (("S", "\"\""), Right Node {rootLabel = R 0, subForest = [Node {rootLabel = R 2, subForest = []}]})
             , (("S", "\"abc\""), Right Node {rootLabel = R 0, subForest = [Node {rootLabel = R 1, subForest = [Node {rootLabel = C 'a', subForest = [Node {rootLabel = R 1, subForest = [Node {rootLabel = C 'b', subForest = [Node {rootLabel = R 1, subForest = [Node {rootLabel = C 'c', subForest = [Node {rootLabel = R 2, subForest = []}]}]}]}]}]}]}]} )
             ]

g4 = stringToGrammar "_0_~_1_$_1_~_2_$_2_~_0_"
e4 = Left (InfiniteLoop (0,Nothing))



main = do
  print $ testTable g1 (Right t1)
  print $ testParse g1 testcases1
  print $ testTable g2 (Right t2)
  print $ testParse g2 testcases2
  print $ testTable g3 (Right t3)
  print $ testParse g3 testcases3
  print $ testTree g3 testtrees3
  print $ testTable g4 e4
  quickCheck (testTreeQC :: Tree Int -> Bool) 
