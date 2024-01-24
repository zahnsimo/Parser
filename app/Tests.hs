module Tests where

import Data.Map hiding (map)

import Control.Monad.Reader
import Control.Monad.Except

import Data.Tree

import Test.QuickCheck

import Grammar
import Main

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

parseForTesting :: ParseInfo -> String -> Either ParseError [ParseStepSimple]
parseForTesting inf s = case runReaderT (parse s) inf of
                          Left e -> Left e
                          Right steps -> Right $ map convParseStep steps

testParse :: Grammar -> [(String, Either ParseError [ParseStepSimple])] ->
  Either [(String, Either ParseError [ParseStepSimple], Either ParseError [ParseStepSimple])] ()
testParse gr testcases
  = let inf = unwrapExcept $ mkParseInfo gr
     in testFunction (parseForTesting inf) testcases

---maybe unimportant, better test for mkTree below

mkParseTreeForTesting :: ParseInfo -> String -> Either ParseError ParseTreeSimple
mkParseTreeForTesting inf s = case runReaderT (parse s) inf of
                                Left e -> Left e
                                Right steps -> Right $ fmap convParseStep $ mkParseTree steps

testTree :: Grammar -> [(String, Either ParseError ParseTreeSimple)] ->
  Either [(String, Either ParseError ParseTreeSimple, Either ParseError ParseTreeSimple)] ()
testTree gr testtrees
  = let inf = unwrapExcept $ mkParseInfo gr
     in testFunction (mkParseTreeForTesting inf) testtrees

---test for trees with quickCheck

children :: Tree a -> Tree (a, Int)
children (Node x xs) = Node (x, length xs) (map children xs)

test_Tree :: Eq a => Tree a -> Bool
test_Tree t = mkTree (flatten ( children t)) == t

----------------testcases----------------------------------

g1 = mkGrammar "ST" [('S', "T"), ('S', "(S+T)"), ('T', "a")]
t1 = fromList [ ((0,Just (TChar "(")) , (1,Rule (0,[TChar "(",NTChar 0,TChar "+",NTChar 1,TChar ")"])))
              , ((0,Just (TChar "a")) , (0,Rule (0,[NTChar 1])))
              , ((1,Just (TChar "a")) , (2,Rule (1,[TChar "a"])))]
testcases1 = [ ("a", Right [R 0,R 2])
             , ("(a+a)", Right [R 1,R 0,R 2,R 2])
             , ("a+a", Left $ StringTooLong "+a")
             , ("(a+", Left $ NoRule 1 "")]

g2 = mkGrammarFromRules [('S', "aa"), ('S', "ab")]
t2 = fromList [ ((0,Just $ TChar "aa") , (0,Rule (0,[TChar "aa"])))
              , ((0,Just $ TChar "ab") , (1,Rule (0,[TChar "ab"])))]
testcases2 = [("aa", Right [R 0]), ("ab", Right [R 1])]

g3 = Grammar 2 ["\"" , "a"] [ ( 0 , Rule ( 0, [TChar "\"" , NTChar 1, TChar "\""]) ) , (1, Rule ( 1, [BL ['\"'], NTChar 1]))  , (2, Rule ( 1, [])) ]
t3 = fromList [ ((0,Just (TChar "\"")) , (0,Rule (0,[TChar "\"",NTChar 1,TChar "\""])))
              , ((1,Nothing) , (2,Rule (1,[])))
              , ((1,Just (BL "\"")) , (1,Rule (1,[BL "\"",NTChar 1])))]
testcases3 = [ ("\"\"", Right [R 0,R 2])
             , ("\"abc\"", Right [R 0,R 1,C 'a',R 1,C 'b',R 1,C 'c',R 2])
             , ("\"", Left $ MissingCharacters "\"")
             , ("a", Left $ NoRule 0 "a") -- 0 [] "a" 
             , ("\"a", Left $ MissingCharacters "\"")]
testtrees3 = [ ("\"\"", Right Node {rootLabel = R 0, subForest = [Node {rootLabel = R 2, subForest = []}]})
             , ("\"abc\"", Right Node {rootLabel = R 0, subForest = [Node {rootLabel = R 1, subForest = [Node {rootLabel = C 'a', subForest = [Node {rootLabel = R 1, subForest = [Node {rootLabel = C 'b', subForest = [Node {rootLabel = R 1, subForest = [Node {rootLabel = C 'c', subForest = [Node {rootLabel = R 2, subForest = []}]}]}]}]}]}]}]} )
             ]


main = do
  print $ testTable g1 (Right t1)
  print $ testParse g1 testcases1
  print $ testTable g2 (Right t2)
  print $ testParse g2 testcases2
  print $ testTable g3 (Right t3)
  print $ testParse g3 testcases3
  print $ testTree g3 testtrees3
  quickCheck (test_Tree :: Tree Int -> Bool) 
