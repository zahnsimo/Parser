module Tests where

import Control.Monad.Reader

import Main

doTests :: Grammar -> [(String, [Int])] -> Bool
doTests gr testcases = let t = mkTable gr
                        in and $ map (\ (s, is) -> runReader (parse s) t == is) testcases

  
g1 = mkGrammarFromRules [('S', "aa"), ('S', "ab")]
t1 = mkTable g1
testcases1 = [("aa", [0]), ("ab", [1])]

test1 = doTests g1 testcases1--runReader (parse "aa") t1 == [0] && runReader (parse "ab") t1 == [1]
