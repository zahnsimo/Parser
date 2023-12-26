{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (lookup)
import Data.Map.Strict hiding (map, filter, take, splitAt, foldr)--as Map
import Control.Monad.Reader
import Data.List hiding (lookup)
import Data.Maybe
import Data.Either
import Control.Monad.Except

import System.IO.Unsafe

import Data.Tree

import Grammar

--------------------types-----------------------


--type Table = Map (NT, Maybe T) (Int, Rule)
type Table = Map (NT, Maybe RuleChar) (Int, Rule)

data TableError = NonDeterministic ((NT, Maybe T), [Int]) | InfiniteLoop (NT, Maybe RuleChar) | Test 
  deriving (Eq, Show)
data ParseError = UnexpectedCharacter | StringTooShort | StringTooLong | NoRule -- NT [RuleChar] String
  | BlackList Char | WhiteList Char
  deriving (Eq, Show)


------------------------mkTable---------------------------------------------------

findRulesWithLevel :: (Monad m, MonadReader Grammar m, MonadError TableError m)
  => Int -> (NT, Maybe RuleChar) -> m [(Int, Rule)]
findRulesWithLevel 0 (i,c) = throwError $ InfiniteLoop (i,c)
findRulesWithLevel l (i, Just c)
  = do rules <- asks rules
       filterM helper rules
         where helper (_, Rule (j,[])) = return False
               helper (_ , Rule (j,s))
                 = do case (i == j, head s == c, head s) of
                        (True, True, _) -> return True
                        (True, False, NTChar k) ->do next_level <- findRulesWithLevel (l-1) (k, Just c)
                                                     return (next_level /= [])
                        (True, _, _) -> return False
                        (False, _,_ )  -> return False

findRulesWithLevel l (i, Nothing)
  = do rules <- asks rules
       filterM helper rules
         where
           helper (_, Rule (j, s)) = if i == j then helper2 s else return False
           helper2 s = case s of
             [] -> return True
             ((NTChar k):xs) -> do next_level <- findRulesWithLevel (l-1) (k, Nothing)
                                   if next_level /= [] then helper2 xs else return False
             _ -> return False


findRules :: (Monad m, MonadReader Grammar m, MonadError TableError m)
  => (NT, Maybe RuleChar) -> m [(Int, Rule)]
findRules (i,c) = do lmax <- asks (length . rules)
                     findRulesWithLevel lmax (i,c)



mkTable :: (Monad m, MonadError TableError m)
  => Grammar -> m Table
mkTable gr@(Grammar n ts rules)
 = fromList <$> catMaybes <$>
  traverse helper [(i, c) | i <- [0..(n-1)] , c <-Nothing : ( map (Just . TChar) ts) ++ ( map (Just . BL) $ bls gr) ++ (map (Just . WL) $ wls gr) ]
  where helper (i,c) = do rls <- runReaderT (findRules (i,c)) gr
                          case rls of [] -> return Nothing
                                      [x] -> return $ Just ((i,c), x)
                                      ls ->  throwError Test -- $ NonDeterministic ((i,c), map fst ls)


-----------------------parse------------------------------------

isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (c:s) (c':s') = c == c' && isPrefix s s'

removePrefix :: String -> String -> Maybe String
removePrefix [] s = Just s
removePrefix (c:s) [] = Nothing
removePrefix (c:s) (c':s') = case c == c' of True -> removePrefix s s'
                                             False -> Nothing



-------extend to BL/WL chars
chooseRuleHelper :: (Monad m, MonadReader Table m, MonadError ParseError m) =>
  NT -> [RuleChar] -> m (Maybe (Int, Rule))
chooseRuleHelper i (t:ts) = do r <- asks $ lookup (i, Just t)
                               case r of Just _ -> return r
                                         Nothing -> chooseRuleHelper i ts
chooseRuleHelper i [] = do r <- asks $ lookup (i, Nothing)
                           return r

downFrom :: Int -> [Int]
downFrom 0 = []
downFrom n = [n, (n-1)..1]

chooseRule :: (Monad m, MonadReader Table m, MonadError ParseError m) =>
  NT -> String -> m (Maybe (Int, Rule))
chooseRule i "" = do r <- asks $ lookup (i, Nothing)
                     return r
chooseRule i s = do ruleChars <- asks $ catMaybes . (map snd) . keys
                    let maxLengthPrefix = maximum $ (map length) $ getTs ruleChars
                    let prefixes = map TChar $ map (\ l -> take l s) $ downFrom maxLengthPrefix
                    let h = head s
                    let wls = map WL $ filter (h `elem`) $ getWLs ruleChars
                    let bls = map BL $ filter (not . (h `elem`)) $ getBLs ruleChars
                    chooseRuleHelper i (prefixes ++ wls ++ bls)
                                       
                       
parse :: (Monad m, MonadReader Table m, MonadError ParseError m) =>
  String -> m [ParseStep]
parse = parseWithStack [NTChar 0]

parseWithStack :: (Monad m, MonadReader Table m, MonadError ParseError m) =>
  [RuleChar] -> String -> m [ParseStep]
parseWithStack (TChar c : xs) s = case removePrefix c s of
  Just s' -> parseWithStack xs s'
  Nothing -> throwError UnexpectedCharacter
parseWithStack (NTChar i :xs) "" = do
  r <- chooseRule i ""
  case r of Just (k, r@(Rule (j, ls))) -> do rest <- parseWithStack (ls ++ xs) []
                                             return ((ParseRule k r):rest)
            Nothing -> throwError StringTooShort
parseWithStack (NTChar i : xs) s = do
  r <- chooseRule i s
  case r of Just (k, r@(Rule (j, ls))) -> do rest <- parseWithStack (ls ++ xs) s
                                             return ((ParseRule k r):rest)
            Nothing -> throwError $ NoRule -- i xs s
parseWithStack (BL cs :xs) (c:s) = do case c `elem` cs of
                                        False -> do rest <- parseWithStack xs s
                                                    return ((ParseChar c):rest)
                                        True -> throwError $ BlackList c
parseWithStack (BL cs :xs) "" = throwError StringTooShort
parseWithStack (WL cs :xs) (c:s) = do case c `elem` cs of
                                        True -> do rest <- parseWithStack xs s
                                                   return ((ParseChar c):rest)
                                        False -> throwError $ WhiteList c
parseWithStack (WL cs :xs) "" = throwError StringTooShort
parseWithStack [] [] = return []
parseWithStack [] _ = throwError StringTooLong



mkTreeWithStack :: [ParseStep] -> [ParseTree] -> ParseTree
mkTreeWithStack (r:rs) trees = case r of
  ParseRule i rule -> let children = countChildren rule
                       in case children of 0 -> let newNode = Node r []
                                                 in mkTreeWithStack rs (newNode:trees)
                                           j -> let (subforest, rest) = splitAt j trees
                                                    newNode = Node r (reverse subforest)
                                                 in mkTreeWithStack rs (newNode : rest)
  ParseChar c -> case trees of (t:ts) -> let newNode = Node r [t]
                                          in mkTreeWithStack rs (newNode : ts)
                               [] -> error "BUG" --maybe extend countchildren
mkTreeWithStack [] trees = case trees of [tree] -> tree
                                         [] -> error "BUG: This should not happen (empty tree)"
                                         _ -> error "BUG: This should not happen (too many trees)"

mkTree :: [ParseStep] -> ParseTree
mkTree rs = mkTreeWithStack (reverse rs) []

testG = mkGrammar "ST" [('S', "T"), ('S', "(S+T)"), ('T', "a")]

testT = case runExcept $ mkTable testG of Right t -> t
                                          Left e -> error $ show e

g = Grammar 2 ["\"" , "a"] [ ( 0 , Rule ( 0, [TChar "\"" , NTChar 1, TChar "\""]) ) , (1, Rule ( 1, [BL ['\"'], NTChar 1]))  , (2, Rule ( 1, [])) ]

t =  case runExcept $ mkTable g of Right ta -> ta
                                   Left e -> error $ show e

main = do
  s <- getLine
  let rules = case (runReaderT (parse s) t) :: Either ParseError [ParseStep] of
        Right r -> r
        Left e -> error $ show e
  print rules
  let tree = mkTree rules
  print tree
  putStr $ drawTree $ fmap show tree
  --print ""

