{-# LANGUAGE TypeFamilies #-}

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

data Init
data Parse

type family T_Info set
type instance T_Info Init = [T]
type instance T_Info Parse = Int

getMaxPrefixLength :: T_Info Init -> T_Info Parse
getMaxPrefixLength = maximum . map length

initToParse :: Alphabet Init -> Alphabet Parse
initToParse (ABC n ts wls bls) = ABC n (getMaxPrefixLength ts) wls bls

data Alphabet set = ABC {
--    n      :: Int
    nts    :: [String]
  , t_info :: T_Info set
  , wls    :: [[Char]]
  , bls    :: [[Char]]
                    }


data Grammar = Grammar {
      abc_init :: Alphabet Init
    , rules :: [(Int,Rule)]
                       }

data ParseStep = ParseRule Int Rule | ParseChar Char
instance Show ParseStep where
  show (ParseRule i r) = show i
  show (ParseChar c) = show c

type ParseTree = Tree ParseStep

---------------------basics grammar fcts---------------------

checkGrammar :: Grammar -> Bool
checkGrammar (Grammar (ABC nts ts wls bls) rules)
  = all (\ (Rule (i, ls)) -> i < n &&
          all (\ s -> case s of NTChar j _ -> j < n
                                TChar s  -> s `elem` ts
                                WildCard True cs -> cs `elem` wls
                                WildCard False cs -> cs `elem` bls)
          ls) $ map snd rules
    where n = length nts

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


rmdups :: (Eq a, Ord a) => [a] -> [a]
rmdups xs = map head $ group $ sort xs

