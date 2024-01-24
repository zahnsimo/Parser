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

type Table = Map (NT, Maybe RuleChar) (Int, Rule)
data ParseInfo = ParseInfo {
    table :: Table
  , stringChars :: [RuleChar]
  , maxPrefixLength :: Int
  }
--contains the table, a list of all RuleChars (excluding NTChars) and maximumPrefixLength

data TableError = NonDeterministic ((NT, Maybe RuleChar), [Int])
                | InfiniteLoop (NT, Maybe RuleChar) --[NT]
                | Test
  deriving (Eq, Show)

data ParseError =
    UnexpectedCharacter Char Char -- returns the expected char vs the actual char
  | MissingCharacters String -- returns the next chars that were expected
  | MissingBLCharacters [Char]
  | MissingWLCharacters [Char]
  | StringTooLong String -- returns the rest of the string
  | NoRule NT String -- expects to parse some rule, returns top NT from stack and rest of string
  | BlackList [Char] Char
  | WhiteList [Char] Char
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
                                      ls ->  throwError $ NonDeterministic ((i,c), map fst ls)

mkParseInfo :: (Monad m, MonadError TableError m)
  => Grammar -> m ParseInfo
mkParseInfo gr@(Grammar n ts rules)
  = do table <- mkTable gr
       let stringChars = catMaybes $ map snd $ keys table
       let maxPrefixLength = maximum $ (map length) $ getTs stringChars
       return $ ParseInfo table stringChars maxPrefixLength

-----------------------parse------------------------------------

isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (c:s) (c':s') = c == c' && isPrefix s s'

removePrefix :: (Monad m, MonadError ParseError m) =>
  String -> String -> m String
removePrefix [] s = return s
removePrefix (c:s) [] = throwError $ MissingCharacters (c:s)
removePrefix (c:s) (c':s') = case c == c' of True -> removePrefix s s'
                                             False -> throwError $ UnexpectedCharacter c c'


chooseRuleHelper :: (Monad m, MonadReader ParseInfo m, MonadError ParseError m) =>
  NT -> [Maybe RuleChar] -> m (Maybe (Int, Rule))
chooseRuleHelper i (t:ts) = do r <- asks $ lookup (i, t) . table
                               case r of Just _ -> return r
                                         Nothing -> chooseRuleHelper i ts
chooseRuleHelper i [] = return Nothing
  -- do r <- asks $ lookup (i, Nothing)
  --                          return r

downFrom :: Int -> [Int]
downFrom 0 = []
downFrom n = [n, (n-1)..1]

chooseRule :: (Monad m, MonadReader ParseInfo m, MonadError ParseError m) =>
  NT -> String -> m (Int, Rule)
chooseRule i "" = do rule <- asks $ lookup (i, Nothing) . table
                     case rule of
                       Just r -> return r
                       Nothing -> throwError $ NoRule i ""
chooseRule i s = do strChars <- asks stringChars
                    maxPrefixLength <- asks maxPrefixLength
                    let prefixes = map TChar $ map (\ l -> take l s) $ downFrom maxPrefixLength
                    let h = head s
                    let wls = map WL $ filter (h `elem`) $ getWLs strChars
                    let bls = map BL $ filter (not . (h `elem`)) $ getBLs strChars
                    rule <- chooseRuleHelper i (map Just (prefixes ++ wls ++ bls) ++ [Nothing])
                    case rule of
                      Just r -> return r
                      Nothing -> throwError $ NoRule i s
                                       
                       
parse :: (Monad m, MonadReader ParseInfo m, MonadError ParseError m) =>
  String -> m [ParseStep]
parse = parseWithStack [NTChar 0]

-- parseWithState :: (Monad m, MonadReader Table m, MonadError ParseError m) =>
--   ([RuleChar], String, [ParseStep]) -> m ([RuleChar], String, [ParseStep])
-- parseWithState ((TChar c : xs) , s , steps) = (\ s' -> parseWithState (xs, s', steps)) =<< removePrefix c s


parseWithStack :: (Monad m, MonadReader ParseInfo m, MonadError ParseError m) =>
  [RuleChar] -> String -> m [ParseStep]
parseWithStack (TChar c : xs) s = parseWithStack xs =<< removePrefix c s
parseWithStack (NTChar i : xs) s = do (k, r@(Rule (j, ls))) <- chooseRule i s
                                      ((ParseRule k r) :) <$> parseWithStack (ls ++ xs) s
parseWithStack (BL cs :xs) (c:s) = do case c `elem` cs of
                                        False -> ((ParseChar c) :) <$> parseWithStack xs s
                                        True  -> throwError $ BlackList cs c
parseWithStack (BL cs :xs) "" = throwError $ MissingBLCharacters cs
parseWithStack (WL cs :xs) (c:s) = do case c `elem` cs of
                                        True  -> ((ParseChar c) :) <$> parseWithStack xs s
                                        False -> throwError $ WhiteList cs c
parseWithStack (WL cs :xs) "" = throwError $ MissingWLCharacters cs
parseWithStack [] [] = return []
parseWithStack [] s = throwError $ StringTooLong s


mkTreeWithStack :: [(a,Int)] -> [Tree a] -> Tree a
mkTreeWithStack ((x, i): xs) trees = let (subforest, rest) = splitAt i trees
                                         newNode = Node x subforest
                                     in mkTreeWithStack xs (newNode : rest)
mkTreeWithStack [] trees = case trees of [tree] -> tree
                                         [] -> error "BUG: This should not happen (empty tree)"
                                         _ -> error "BUG: This should not happen (too many trees)"

mkTree :: [(a,Int)] -> Tree a
mkTree rs = mkTreeWithStack (reverse rs) []


mkParseTree :: [ParseStep] -> ParseTree
mkParseTree = mkTree . map (\ step -> (step, countChildren step) )



testG = mkGrammar "ST" [('S', "T"), ('S', "(S+T)"), ('T', "a")]

testT = case runExcept $ mkTable testG of Right t -> t
                                          Left e -> error $ show e

testI = case runExcept $ mkParseInfo testG of Right t -> t
                                              Left e -> error $ show e

g = Grammar 2 ["\"" , "a"] [ ( 0 , Rule ( 0, [TChar "\"" , NTChar 1, TChar "\""]) ) , (1, Rule ( 1, [BL ['\"'], NTChar 1]))  , (2, Rule ( 1, [])) ]

t =  case runExcept $ mkTable g of Right ta -> ta
                                   Left e -> error $ show e

i = case runExcept $ mkParseInfo testG of Right t -> t
                                          Left e -> error $ show e

main = do
  s <- getLine
  let rules = case (runReaderT (parse s) testI) :: Either ParseError [ParseStep] of
        Right r -> r
        Left e -> error $ show e
  print rules
  let tree = mkParseTree rules
  --print tree
  putStr $ drawTree $ fmap show tree
  print $ flatten tree
  --print ""

