{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (lookup)
import Data.Map.Strict hiding (map, filter)--as Map
import Control.Monad.Reader
import Data.List hiding (lookup)
import Data.Maybe
import Data.Either
import Control.Monad.Except

--------------------types-----------------------

type NT = Int
type T = String --Char

data RuleChar = NTChar NT  | TChar T
  deriving (Show, Eq)

newtype Rule = Rule (NT, [RuleChar])
  deriving (Show, Eq)


data Grammar = Grammar {
      n     :: Int    -- number of NTs -> assumed as [0..n-1]
    , ts    :: [T]
    --ts = findTs . rules
    --start
    , rules :: [(Int,Rule)]
                       }
  deriving Show

--ts :: Grammar -> [T]


type Table = Map (NT, Maybe T) (Int, Rule)

data TableError = NonDeterministic ((NT, Maybe T), [Int]) | InfiniteLoop (NT, Maybe T)
  deriving Show
data ParseError = UnexpectedCharacter | StringTooShort | StringTooLong | NoRule
  deriving Show

---------------------basics grammar fcts---------------------

checkGrammar :: Grammar -> Bool
checkGrammar (Grammar n ts rules) = all (\ (Rule (i, ls)) -> i < n &&
                                            all (\ s -> case s of NTChar j -> j < n
                                                                  TChar s  -> s `elem` ts) ls)
                                    $ map snd rules


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

------------------------mkTable---------------------------------------------------

findRulesWithLevel :: (Monad m, MonadReader Grammar m, MonadError TableError m)
  => Int -> (NT, Maybe T) -> m [(Int, Rule)]
findRulesWithLevel 0 (i,c) = throwError $ InfiniteLoop (i,c)
findRulesWithLevel l (i, Just c)
  = do rules <- asks rules
       filterM helper rules
         where helper (_, Rule (j,[])) = return False
               helper (_ , Rule (j,s)) = do case (i == j, head s) of
                                              (True, TChar c') -> return $ c == c'
                                              (True, NTChar k) -> do next_level <- findRulesWithLevel (l-1) (k, Just c)
                                                                     return (next_level /= [])
                                              (False, _ )  -> return False

findRulesWithLevel l (i, Nothing)
  = do rules <- asks rules
       filterM helper rules
         where
           helper (_, Rule (j, s)) = if i == j then helper2 s else return False
           helper2 s = case s of
             [] -> return True
             ((TChar c):xs)  -> return False
             ((NTChar k):xs) -> do next_level <- findRulesWithLevel (l-1) (k, Nothing)
                                   if next_level /= [] then helper2 xs else return False

findRules :: (Monad m, MonadReader Grammar m, MonadError TableError m)
  => (NT, Maybe T) -> m [(Int, Rule)]
findRules (i,c) = do lmax <- asks (length . rules)
                     findRulesWithLevel lmax (i,c)



mkTable :: (Monad m, MonadError TableError m)
  => Grammar -> m Table
mkTable gr@(Grammar n ts rules)
 = fromList <$> catMaybes <$>
  traverse helper [(i, c) | i <- [0..(n-1)] , c <- Nothing : map Just ts]
  where helper (i,c) = do rls <- runReaderT (findRules (i,c)) gr
                          case rls of [] -> return Nothing
                                      [x] -> return $ Just ((i,c), x)
                                      ls ->  throwError $ NonDeterministic ((i,c), map fst ls)

-- ---------------------parse------------------------------------

isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (c:s) (c':s') = c == c' && isPrefix s s'

removePrefix :: String -> String -> Maybe String
removePrefix [] s = Just s
removePrefix (c:s) [] = Nothing
removePrefix (c:s) (c':s') = case c == c' of True -> removePrefix s s'
                                             False -> Nothing

chooseRuleHelper :: (Monad m, MonadReader Table m, MonadError ParseError m) =>
  NT -> [T] -> m (Maybe (Int, Rule))
chooseRuleHelper i (t:ts) = do r <- asks $ lookup (i, Just t)
                               case r of Just _ -> return r
                                         Nothing -> chooseRuleHelper i ts
chooseRuleHelper _ [] = return Nothing

chooseRuleNew :: (Monad m, MonadReader Table m, MonadError ParseError m) =>
  NT -> String -> m (Maybe (Int, Rule))
chooseRuleNew i s = do ts <- asks $ catMaybes . map snd . keys
                       let prefixes = sortBy (\ p p' -> compare (length p') (length p))
                                       $ filter (\ p -> isPrefix p s) ts
                       chooseRuleHelper i prefixes
                       

-- --chooseRule :: NT -> Maybe T -> Reader Table (Maybe (Int, Rule))
-- -- chooseRule i (Just c) = do r <- asks $ lookup (i, Just c)
-- --                            case r of Just _  -> return r
-- --                                      Nothing -> chooseRule i Nothing
-- -- chooseRule i Nothing = do r <- asks $ lookup (i, Nothing)
-- --                           case r of Just _  -> return r
-- --                                     Nothing -> return Nothing

-- --chooseRule i c = asks $ lookup (i, c)

parse :: (Monad m, MonadReader Table m, MonadError ParseError m) =>
  String -> m [Int]
parse = parseWithStack [NTChar 0]

parseWithStack :: (Monad m, MonadReader Table m, MonadError ParseError m) =>
  [RuleChar] -> String -> m [Int]
parseWithStack (TChar c : xs) s = case removePrefix c s of
  Just s' -> parseWithStack xs s'
  Nothing -> throwError UnexpectedCharacter
parseWithStack (NTChar i :xs) "" = do
  r <- chooseRuleNew i ""
  case r of Just (k, Rule (j, ls)) -> do rest <- parseWithStack (ls ++ xs) []
                                         return (k:rest)
            Nothing -> throwError StringTooShort
parseWithStack (NTChar i : xs) s = do
  r <- chooseRuleNew i s
  case r of Just (k, Rule (j, ls)) -> do rest <- parseWithStack (ls ++ xs) s
                                         return (k:rest)
            Nothing -> throwError NoRule
parseWithStack [] [] = return []
parseWithStack [] _ = throwError StringTooLong


testG = mkGrammar "ST" [('S', "T"), ('S', "(S+T)"), ('T', "a")]

testT = case runExcept $ mkTable testG of Right t -> t
                                          Left e -> error $ show e

main = do
  s <- getLine
  let rules = runExceptT $ runReaderT (parse s) testT
  print $ fromRight undefined $ rules
  print ""

