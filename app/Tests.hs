module Tests where

import Data.Map hiding (map)

import Control.Monad.Reader
import Control.Monad.Except

import Data.Tree

import Grammar
import Main

data ParseStepSimple = I Int | C Char
  deriving Eq

convParseStep :: ParseStep -> ParseStepSimple
convParseStep (ParseRule i r) = I i
convParseStep (ParseChar c) = C c

type ParseTreeSimple = Tree ParseStepSimple

testTable :: Grammar -> Either TableError Table -> Bool
testTable gr exp = runExcept (mkTable gr) == exp

unwrapTable table = case runExcept table of Right t -> t
                                            Left e -> error $ show e

testParse :: Grammar -> [(String, Either ParseError [ParseStepSimple])] -> Bool
testParse gr testcases
  = let t = unwrapTable $ mkTable gr
        compareRes :: (String, Either ParseError [ParseStepSimple]) -> Bool
        compareRes (s, exp) = case (runReaderT (parse s) t, exp) of
                                (Right rules, Right rules') -> map convParseStep rules == rules'
                                (Left e, Left e') -> e == e'
                                (_, _) -> False
     in and $ map compareRes testcases

testTree :: Grammar -> [(String, Either ParseError ParseTreeSimple)] -> Bool
testTree gr testtrees
  = let t = unwrapTable $ mkTable gr
        compareTrees :: (String, Either ParseError ParseTreeSimple) -> Bool
        compareTrees (s, exp) = case (runReaderT (mkTree <$> parse s) t, exp) of
                                  (Right tree, Right tree') -> fmap convParseStep tree == tree'
                                  (Left e, Left e') -> e == e'
                                  (_, _) -> False
    in and $ map compareTrees testtrees

g1 = mkGrammar "ST" [('S', "T"), ('S', "(S+T)"), ('T', "a")]
t1 = fromList [ ((0,Just (TChar "(")) , (1,Rule (0,[TChar "(",NTChar 0,TChar "+",NTChar 1,TChar ")"])))
              , ((0,Just (TChar "a")) , (0,Rule (0,[NTChar 1])))
              , ((1,Just (TChar "a")) , (2,Rule (1,[TChar "a"])))]
testcases1 = [ ("a", Right [I 0,I 2])
             , ("(a+a)", Right [I 1,I 0,I 2,I 2])
             , ("a+a", Left StringTooLong)
             , ("(a+", Left StringTooShort)]

g2 = mkGrammarFromRules [('S', "aa"), ('S', "ab")]
t2 = fromList [ ((0,Just $ TChar "aa") , (0,Rule (0,[TChar "aa"])))
              , ((0,Just $ TChar "ab") , (1,Rule (0,[TChar "ab"])))]
testcases2 = [("aa", Right [I 0]), ("ab", Right [I 1])]

g3 = Grammar 2 ["\"" , "a"] [ ( 0 , Rule ( 0, [TChar "\"" , NTChar 1, TChar "\""]) ) , (1, Rule ( 1, [BL ['\"'], NTChar 1]))  , (2, Rule ( 1, [])) ]
t3 = fromList [ ((0,Just (TChar "\"")) , (0,Rule (0,[TChar "\"",NTChar 1,TChar "\""])))
              , ((1,Nothing) , (2,Rule (1,[])))
              , ((1,Just (BL "\"")) , (1,Rule (1,[BL "\"",NTChar 1])))]
testcases3 = [ ("\"\"", Right [I 0,I 2])
             , ("\"abc\"", Right [I 0,I 1,C 'a',I 1,C 'b',I 1,C 'c',I 2])
             , ("\"", Left UnexpectedCharacter)
             , ("a", Left NoRule) -- 0 [] "a" 
             , ("\"a", Left UnexpectedCharacter)]
testtrees3 = [ ("\"\"", Right Node {rootLabel = I 0, subForest = [Node {rootLabel = I 2, subForest = []}]})
             , ("\"abc\"", Right Node {rootLabel = I 0, subForest = [Node {rootLabel = I 1, subForest = [Node {rootLabel = C 'a', subForest = [Node {rootLabel = I 1, subForest = [Node {rootLabel = C 'b', subForest = [Node {rootLabel = I 1, subForest = [Node {rootLabel = C 'c', subForest = [Node {rootLabel = I 2, subForest = []}]}]}]}]}]}]}]} )
             ]


main = do
  print $ testTable g1 (Right t1)
  print $ testParse g1 testcases1
  print $ testTable g2 (Right t2)
  print $ testParse g2 testcases2
  print $ testTable g3 (Right t3)
  print $ testParse g3 testcases3
  print $ testTree g3 testtrees3
