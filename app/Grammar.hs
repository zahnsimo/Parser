module Grammar where

import Data.List
import Data.Maybe
import Data.Tree

type NT = Int
type T = String

data RuleChar = NTChar NT Bool -- true: NT appears in parse tree; false: NT hidden
              | TChar T
              | WildCard Bool [Char] -- true for WhiteList (c in list)
                                     -- false for BlackList (c notin list)
  deriving (Show, Eq, Ord)

newtype Rule = Rule (NT, [RuleChar])
  deriving (Show, Eq)


data Grammar = Grammar {
      n     :: Int    -- number of NTs -> assumed as [0..n-1]
    , ts    :: [T]
    , wls   :: [[Char]]
    , bls   :: [[Char]]
    , rules :: [(Int,Rule)]
                       }
--  deriving Show

data ParseStep = ParseRule Int Rule | ParseChar Char
instance Show ParseStep where
  show (ParseRule i r) = show i
  show (ParseChar c) = show c

type ParseTree = Tree ParseStep

---------------------basics grammar fcts---------------------

checkGrammar :: Grammar -> Bool
checkGrammar (Grammar n ts wls bls rules)
  = all (\ (Rule (i, ls)) -> i < n &&
          all (\ s -> case s of NTChar j _ -> j < n
                                TChar s  -> s `elem` ts
                                WildCard True cs -> cs `elem` wls
                                WildCard False cs -> cs `elem` bls)
          ls) $ map snd rules

-- counts the number of children a rule has in the parse tree
countChildren :: ParseStep -> Int
countChildren (ParseRule i r) = length $ filter isNTVisible $ getRHS r
countChildren (ParseChar c) = 1

--- filter for certain types of RuleChar

isNTVisible :: RuleChar -> Bool
isNTVisible (NTChar _ True) = True
isNTVisible _ = False

isNTHidden :: RuleChar -> Bool
isNTHidden (NTChar _ False) = True
isNTHidden _ = False

isNT :: RuleChar -> Bool
isNT (NTChar _ _) = True
isNT _ = False

isT :: RuleChar -> Bool
isT (TChar _) = True
isT _ = False

isWC :: Bool -> RuleChar -> Bool
isWC b' (WildCard b _) = b==b'
isWC _ _ = False

getRHS :: Rule -> [RuleChar]
getRHS (Rule (i, cs)) = cs

getNTs :: [RuleChar] -> [NT]
getNTs = rmdups . map (\ (NTChar i _) -> i) . filter isNT

getNTVisibles :: [RuleChar] -> [NT]
getNTVisibles = rmdups . map (\ (NTChar i _) -> i) . filter isNTVisible

getTs :: [RuleChar] -> [T]
getTs = rmdups. map (\ (TChar s) -> s) . filter isT

getWCs :: Bool -> [RuleChar] -> [[Char]]
getWCs b = rmdups. map (\ (WildCard _ cs) -> cs) . filter (isWC b)

---convert strings to rules

rmdups :: (Eq a, Ord a) => [a] -> [a]
rmdups xs = map head $ group $ sort xs

convertToRuleChars :: [Char] -> String -> [RuleChar]
convertToRuleChars ntStr rStr = let (a,b) = break (\ c -> c `elem` ntStr) rStr
                                    in case (a, b) of
                                         ([], []) -> []
                                         (a, []) -> [TChar a]
                                         ([], c:cs) -> (NTChar (fromJust $ elemIndex c ntStr) True) : convertToRuleChars ntStr cs
                                         (a, c:cs) -> (TChar a) : (NTChar (fromJust $ elemIndex c ntStr) True): convertToRuleChars ntStr cs

stringToRule :: [Char] -> (Char, String) -> Rule
stringToRule ntStr (c, rStr) = case elemIndex c ntStr of
                                 Just i -> Rule (i, convertToRuleChars ntStr rStr)
                                 Nothing -> error "LHS of rule is not a NTChar"

--makes Grammar from list of nonterminals and list of rules
mkGrammar :: [Char] -> [(Char, String)] -> Grammar
mkGrammar ntStr rStrs = let n = length ntStr
                            rules = zip [0..] $ map (stringToRule ntStr) rStrs
                            ts = rmdups $ concatMap (getTs . getRHS . snd) $ rules
                         in (Grammar n ts [] [] rules)


--- makes Grammar only from list of rules (assumes the nonterminals are all symbols on lhs of rules)
mkGrammarFromRules :: [(Char, String)] -> Grammar
mkGrammarFromRules rStrs = let ntStr = rmdups $ map fst rStrs
                           in mkGrammar ntStr rStrs 
