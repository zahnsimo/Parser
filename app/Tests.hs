module Tests where

import Data.Map hiding (map)

import Control.Monad.Reader
import Control.Monad.Except

import Grammar
import Main

testTable :: Grammar -> Either TableError Table -> Bool
testTable gr exp = runExcept (mkTable gr) == exp

unwrapTable table = case runExcept table of Right t -> t
                                            Left e -> error $ show e

testParse :: Grammar -> [(String, Either ParseError [Int])] -> Bool
testParse gr testcases
  = let t = unwrapTable $ mkTable gr
        compareRes :: (String, Either ParseError [Int]) -> Bool
        compareRes (s, exp) = case (runReaderT (parse s) t, exp) of
                                (Right rules, Right rules') -> map fst rules == rules'
                                (Left e, Left e') -> e == e'
                                (_, _) -> False
     in and $ map compareRes testcases

g1 = mkGrammar "ST" [('S', "T"), ('S', "(S+T)"), ('T', "a")]
t1 = fromList [((0,Just "("),(1,Rule (0,[TChar "(",NTChar 0,TChar "+",NTChar 1,TChar ")"]))),((0,Just "a"),(0,Rule (0,[NTChar 1]))),((1,Just "a"),(2,Rule (1,[TChar "a"])))]
testcases1 = [ ("a", Right [0,2])
             , ("(a+a)", Right [1,0,2,2])
             , ("a+a", Left StringTooLong)
             , ("(a+", Left StringTooShort)]

g2 = mkGrammarFromRules [('S', "aa"), ('S', "ab")]
t2 = fromList [ ((0,Just "aa") , (0,Rule (0,[TChar "aa"])))
              , ((0,Just "ab") , (1,Rule (0,[TChar "ab"])))]
testcases2 = [("aa", Right [0]), ("ab", Right [1])]


main = do
  print $ testTable g1 (Right t1)
  print $ testParse g1 testcases1
  print $ testTable g2 (Right t2)
  print $ testParse g2 testcases2
