module Main where

import Prelude hiding (lookup)
import Data.Map.Strict hiding (map, filter)--as Map
import Control.Monad.Reader
import Data.List hiding (lookup)
import Data.Maybe


---------------------types-----------------------

type NT = Int
type T = Char

data RuleChar = NTChar NT  | TChar T
  deriving (Show, Eq)

newtype Rule = Rule (NT, [RuleChar])
  deriving (Show, Eq)


data Grammar = Grammar {
      n     :: Int    -- number of NTs -> assumed as [0..n-1]
    , ts    :: [Char] --[String]
    , rules :: [(Int,Rule)]
                       }
  deriving Show


type Table = Map (Int, Maybe Char) (Int, Rule)

data TableError = NonDeterministic 

---------------------basics grammar fcts---------------------

checkGrammar :: Grammar -> Bool
checkGrammar (Grammar n ts rules) = all (\ (Rule (i, ls)) -> i < n &&
                                            all (\ s -> case s of NTChar j -> j < n
                                                                  TChar s  -> s `elem` ts) ls)
                                    $ map snd rules

charToSymbol :: [Char] -> [Char] -> Char -> Maybe RuleChar
charToSymbol nstr tstr c = case (c `elemIndex` nstr, c `elemIndex` tstr) of
                         (Just i, Just j)   -> error "Overlap in NTs and Ts"
                         (Just i , Nothing) -> Just (NTChar i)
                         (Nothing, Just _)  -> Just (TChar c)
                         (Nothing, Nothing) -> Nothing


stringToRule :: [Char] -> [Char] -> (Char, String) -> Rule
stringToRule nstr tstr r = case (elemIndex (fst r) nstr , mapM (charToSymbol nstr tstr) (snd r)) of
                            (Just i, Just s)   -> Rule (i, s)
                            (Just i, Nothing)  -> error "Rule (RHS) contains character that is neither NT nor T"
                            (Nothing, Just s)  -> error "Rule (LHS) is not an NT"
                            (Nothing, Nothing) -> error "Rule (LHS & RHS) contains character that is neither NT nor T"


mkGrammar :: [Char] -> [Char] -> [(Char,String)] -> Grammar
mkGrammar nstr tstr rstr | any (\ c -> c `elem` tstr) nstr = error "Overlap in NTs and Ts"
                         | otherwise = let n   = length nstr 
                                           ts  = tstr
                                           rules = zip [0..] $ map (stringToRule nstr tstr) rstr
                                       in (Grammar n ts rules)


------------------------mkTable---------------------------------------------------

findRulesWithLevel :: Int -> (NT, Maybe T) -> Reader Grammar [(Int, Rule)]
findRulesWithLevel 0 _ = error "Infinite loop"
findRulesWithLevel l (i, Just c)
  = do gr <- ask
       rules <- asks rules
       let rls = filter helper rules
             where helper (_, Rule (j,[])) = False
                   helper (_ , Rule (j,s)) = case (i == j, head s) of
                     (True, TChar c') -> c == c'
                     (True, NTChar k) -> runReader (findRulesWithLevel (l-1) (k, Just c)) gr /= []
                     (False, _ )  -> False
        in return rls
findRulesWithLevel l (i, Nothing)
  = do gr <- ask
       rules <- asks rules
       let rls = filter helper rules
             where helper (_, Rule (j, s)) = case (i == j) of
                     False -> False
                     True -> helper2 s
                       where helper2 [] = True
                             helper2 (TChar c : s) = False
                             helper2 (NTChar k : s) = runReader (findRulesWithLevel (l-1) (k, Nothing)) gr /= [] && helper2 s
        in return rls
                   

findRules :: (NT, Maybe T) -> Reader Grammar [(Int, Rule)]
findRules (i,c) = do gr <- ask
                     lmax <- asks (length . rules)
                     return $ runReader (findRulesWithLevel lmax (i,c)) gr

mkTable :: Grammar -> Table --Either TableError Table
mkTable gr@(Grammar n ts rules) = fromList $ catMaybes $ map helper ([(i,Just c) | i <- [0..(n-1)] , c <- ts] ++ [(i, Nothing) | i <- [0..(n-1)]])
  where helper (i,c) = let rls = runReader (findRules (i,c)) gr
                       in case length rls of 0 -> Nothing
                                             1 -> Just ((i,c), head rls)
                                             _ -> error "Table is nondeterministic (multiple fitting rules)"


---------------------parse------------------------------------

chooseRule :: NT -> Maybe T -> Reader Table (Maybe (Int, Rule))
chooseRule i (Just c) = do r <- asks $ lookup (i, Just c)
                           case r of Just _  -> return r
                                     Nothing -> chooseRule i Nothing
chooseRule i Nothing = do r <- asks $ lookup (i, Nothing)
                          case r of Just _  -> return r
                                    Nothing -> return Nothing
  
parse :: String -> Reader Table [Int]
parse = parseWithStack [NTChar 0]

parseWithStack :: [RuleChar] -> String -> Reader Table [Int]
parseWithStack (TChar c : xs) (c':s) = case c == c' of
  True -> parseWithStack xs s
  False -> error "Unexpected character"
parseWithStack (TChar c : xs) [] = error "String ended too early"
parseWithStack (NTChar i : xs) (c:s) = do
  r <- chooseRule i (Just c)
  case r of Just (k, Rule (j, ls)) -> do rest <- parseWithStack (ls ++ xs) (c:s)
                                         return (k:rest)
            Nothing -> error "there is no rule for this state"
parseWithStack (NTChar i :xs) [] = do
  r <- chooseRule i Nothing
  case r of Just (k, Rule (j, ls)) -> do rest <- parseWithStack (ls ++ xs) []
                                         return (k:rest)
            Nothing -> error "String ended too early"
parseWithStack [] [] = return []
parseWithStack [] _ = error "String too long"


testG = mkGrammar "ST" "()a+" [('S', "T"), ('S', "(S+T)"), ('T', "a")]

testT = mkTable testG

main = do
  s <- getLine
  let rules = runReader (parse s) testT
  print rules
  --print ""

