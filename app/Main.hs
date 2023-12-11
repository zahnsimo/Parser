module Main where

import Prelude hiding (lookup)
import Data.Map.Strict hiding (map, filter)--as Map
import Control.Monad.Reader
import Data.List hiding (lookup)
import Data.Maybe

data Symbol = NT Int | T Char --String
  deriving (Show, Eq)

data Rule = Rule (Int, [Symbol])
  deriving (Show, Eq)

type NumberedRules = Map Int Rule

data Grammar = Grammar {
      n     :: Int    -- number of NTs -> assumed as [0..n-1]
    , ts    :: [Char] --[String]
    , rules :: NumberedRules
                       }
  deriving Show


type Table = Map (Int, Maybe Char) (Int, Rule)
--type EpsilonTable = Map Int Int

checkGrammar :: Grammar -> Bool
checkGrammar (Grammar n ts rules) = all (\ (Rule (i, ls)) -> i < n &&
                                            all (\ s -> case s of NT j -> j < n
                                                                  T s  -> s `elem` ts) ls) rules

charToSymbol :: [Char] -> [Char] -> Char -> Maybe Symbol
charToSymbol nstr tstr c = case (c `elemIndex` nstr, c `elemIndex` tstr) of
                         (Just i, Just j)   -> error "Overlap in NTs and Ts"
                         (Just i , Nothing) -> Just (NT i)
                         (Nothing, Just _)  -> Just (T c)
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
                                           rules = fromList $ zip [0..] $ map (stringToRule nstr tstr) rstr
                                       in (Grammar n ts rules)

-- mkGrammar :: [Rule] -> Grammar

is_nt :: Symbol -> Bool
is_nt (NT _) = True
is_nt (T _) = False


findRulesWithLevel :: Int -> (Int, Maybe Char) -> Reader Grammar [(Int, Rule)]
findRulesWithLevel 0 _ = error "Infinite loop"
findRulesWithLevel l (i, Just c)
  = do gr <- ask
       rules <- asks rules
       let rls = filter helper $ toList rules
             where helper (_, Rule (j,[])) = False
                   helper (_ , Rule (j,s)) = case (i == j, head s) of
                     (True, T c') -> c == c'
                     (True, NT k) -> runReader (findRulesWithLevel (l-1) (k, Just c)) gr /= []
                     (False, _ )  -> False
        in return rls
findRulesWithLevel l (i, Nothing)
  = do gr <- ask
       rules <- asks rules
       let rls = filter helper $ toList rules
             where helper (_, Rule (j, s)) = case (i == j) of
                     False -> False
                     True -> helper2 s
                       where helper2 [] = True
                             helper2 (T c : s) = False
                             helper2 (NT k : s) = runReader (findRulesWithLevel (l-1) (k, Nothing)) gr /= [] && helper2 s
        in return rls
                   

findRules :: (Int, Maybe Char) -> Reader Grammar [(Int, Rule)]
findRules (i,c)= do gr <- ask
                    lmax <- asks (length . toList . rules)
                    return $ runReader (findRulesWithLevel lmax (i,c)) gr

mkTable :: Grammar -> Table
mkTable gr@(Grammar n ts rules) = fromList $ catMaybes $ map helper ([(i,Just c) | i <- [0..(n-1)] , c <- ts] ++ [(i, Nothing) | i <- [0..(n-1)]])
  where helper (i,c) = let rls = runReader (findRules (i,c)) gr
                       in case length rls of 0 -> Nothing
                                             1 -> Just ((i,c), head rls)
                                             _ -> error "Table is nondeterministic (multiple fitting rules)"

parse :: String -> Reader Table [Int]
parse = parseWithStack [NT 0]

--applyRule :: [Symbol] -> Rule -> [Symbol]

parseWithStack :: [Symbol] -> String -> Reader Table [Int]
parseWithStack (NT i : xs) (c:s) = do
  r <- asks $ lookup (i, Just c)
  case r of Just (k, (Rule (j, ls))) -> do rest <- parseWithStack (ls ++ xs) (c:s)
                                           return (k:rest)
            Nothing -> do r' <- asks $ lookup (i, Nothing)
                          case r' of Just (k', Rule (j, ls)) -> do rest <- parseWithStack (ls ++ xs) (c:s)
                                                                   return (k':rest)
                                     Nothing -> error "there is no rule for this state"
parseWithStack (T c : xs) (c':s) = case c == c' of
  True -> parseWithStack xs s
  False -> error "Unexpected character"
parseWithStack [] [] = return []
parseWithStack [] _ = error "String too long"
parseWithStack _ [] = error "String ended too early"

testG = mkGrammar "ST" "()a+" [('S', "T"), ('S', "(S+T)"), ('T', "a")]

testNR = rules testG

--testT = fromList [((0,Just '(') , 1 ) , ((0, Just 'a') , 0 ) , ((1, Just 'a') , 2) ] :: Table

main = do
  s <- getLine
  --let rules = runReader ( parse s) (testT , testNR)
  --print rules
  print ""

