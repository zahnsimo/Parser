module Grammar where

import Data.List
import Data.Maybe
import Data.Tree

type NT = Int
type T = String

data RuleChar = NTChar NT  | TChar T | BL [Char] | WL [Char]
  deriving (Show, Eq, Ord)

newtype Rule = Rule (NT, [RuleChar])
  deriving (Show, Eq)


data Grammar = Grammar {
      n     :: Int    -- number of NTs -> assumed as [0..n-1]
    , ts    :: [T]
    , rules :: [(Int,Rule)]
                       }
  deriving Show

data ParseStep = ParseRule Int Rule | ParseChar Char
instance Show ParseStep where
  show (ParseRule i r) = show i
  show (ParseChar c) = show c

type ParseTree = Tree ParseStep

---------------------basics grammar fcts---------------------

checkGrammar :: Grammar -> Bool
checkGrammar (Grammar n ts rules) = all (\ (Rule (i, ls)) -> i < n &&
                                            all (\ s -> case s of NTChar j -> j < n
                                                                  TChar s  -> s `elem` ts) ls)
                                    $ map snd rules

-- counts the number of children a rule has in the parse tree
countChildren :: ParseStep -> Int
countChildren (ParseRule i r) = length $ getNTs $ getRHS r
countChildren (ParseChar c) = 1

--- find BLs and WLs

getRHS :: Rule -> [RuleChar]
getRHS (Rule (i, cs)) = cs

getTs :: [RuleChar] -> [T]
getTs = rmdups. foldr (\ rc xs -> case rc of
                                    TChar cs -> cs : xs
                                    _ -> xs) []

getNTs :: [RuleChar] -> [NT]
getNTs = rmdups . foldr (\ rc xs -> case rc of
                                      NTChar cs -> cs : xs
                                      _ -> xs) []

getBLs :: [RuleChar] -> [[Char]]
getBLs = rmdups . foldr (\ rc xs -> case rc of
                                      BL cs -> cs : xs
                                      _ -> xs) []

getWLs :: [RuleChar] -> [[Char]]
getWLs = rmdups . foldr (\ rc xs -> case rc of
                                      WL cs -> cs : xs
                                      _ -> xs) []

bls :: Grammar -> [[Char]]
bls gr = rmdups . concatMap (getBLs . getRHS . snd) $ rules gr

wls :: Grammar -> [[Char]]
wls gr = rmdups . concatMap (getWLs . getRHS . snd) $ rules gr

---convert strings to rules

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
                                 Nothing -> error "LHS of rule is not a NTChar"

--- makes Grammar from list of nonterminals and list of rules
mkGrammar :: [Char] -> [(Char, String)] -> Grammar
mkGrammar ntStr rStrs = let n = length ntStr
                            rules = zip [0..] $ map (stringToRule ntStr) rStrs
                            ts = rmdups $ concatMap (getTs . getRHS . snd) $ rules
                         in (Grammar n ts rules)


--- makes Grammar only from list of rules (assumes the nonterminals are all symbols on lhs of rules)
mkGrammarFromRules :: [(Char, String)] -> Grammar
mkGrammarFromRules rStrs = let ntStr = rmdups $ map fst rStrs
                           in mkGrammar ntStr rStrs 
