
module Grammar where

import Data.List
import Data.Maybe
import Data.Tree

type NT = Int
type T = String --Char

data RuleChar = NTChar NT  | TChar T
  deriving (Show, Eq)

newtype Rule = Rule (NT, [RuleChar])
  deriving (Show, Eq)


data Grammar = Grammar {
      n     :: Int    -- number of NTs -> assumed as [0..n-1]
    , ts    :: [T]
    , rules :: [(Int,Rule)]
                       }
  deriving Show

type ParseTree = Tree Int

---------------------basics grammar fcts---------------------

checkGrammar :: Grammar -> Bool
checkGrammar (Grammar n ts rules) = all (\ (Rule (i, ls)) -> i < n &&
                                            all (\ s -> case s of NTChar j -> j < n
                                                                  TChar s  -> s `elem` ts) ls)
                                    $ map snd rules

-- counts the number of children a rule has in the parse tree
countChildren :: Rule -> Int
countChildren (Rule r) = length $ filter (\ c -> case c of NTChar _ -> True
                                                           TChar _ -> False) $ snd r

rmdups :: (Eq a, Ord a) => [a] -> [a]
rmdups xs = map head $ groupBy (\ x y -> x == y) $ sort xs

convertToRuleChars :: [Char] -> String -> [RuleChar]
convertToRuleChars ntStr rStr = let (a,b) = break (\ c -> c `elem` ntStr) rStr
                                    in case (a, b) of
                                         ([], []) -> []
                                         (a, []) -> [TChar a]
                                         ([], c:cs) -> (NTChar (fromJust $ elemIndex c ntStr)) : convertToRuleChars ntStr cs
                                         (a, c:cs) -> (TChar a) : (NTChar (fromJust $ elemIndex c ntStr)): convertToRuleChars ntStr cs

stringToRule :: [Char] -> (Char, String) -> Rule
stringToRule ntStr (c, rStr) = case elemIndex c ntStr of
                                 Just i -> Rule (i, convertToRuleChars ntStr rStr)
                                 Nothing -> error "blub"

toT :: RuleChar -> Maybe T
toT (NTChar i) = Nothing
toT (TChar s) = Just s

findTs :: [(Int, Rule)] -> [T]
findTs rules = rmdups $ catMaybes $ map toT =<< map (\ (k, Rule (j, rhs)) -> rhs) rules

--- makes Grammar from list of nonterminals and list of rules
mkGrammar :: [Char] -> [(Char, String)] -> Grammar
mkGrammar ntStr rStrs = let n = length ntStr
                            rules = zip [0..] $ map (stringToRule ntStr) rStrs
                            ts = findTs rules
                         in (Grammar n ts rules)

--- makes Grammar only from list of rules (assumes the nonterminals are all symbols on lhs of rules)
mkGrammarFromRules :: [(Char, String)] -> Grammar
mkGrammarFromRules rStrs = let ntStr = rmdups $ map fst rStrs
                           in mkGrammar ntStr rStrs 
