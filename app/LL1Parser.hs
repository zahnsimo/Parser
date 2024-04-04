{-# LANGUAGE FlexibleContexts #-}

module LL1Parser where

import Prelude hiding (lookup)
import Data.Map.Strict hiding (map, filter, take, splitAt, foldr)--as Map
import Control.Monad.Reader
import Data.List hiding (lookup)
import Data.Maybe
import Data.Either
import Control.Monad.Except

--import System.IO.Unsafe

import Data.Tree

import Grammar

--------------------types-----------------------

type Table = Map (NT, Maybe RuleChar) (Int, Rule)
data ParseInfo = ParseInfo {
      table :: Table
    , abc_parse :: Alphabet Parse
  }
--  deriving Show
--abc_parse contains number of NTs, wls , bls and maxPrefixLength of Ts

data TableError = NonDeterministic ((NT, Maybe RuleChar), [Int])
                | InfiniteLoop (NT, Maybe RuleChar) --[NT]
                | Test
  deriving (Eq, Show)

data ParseError =
    UnexpectedCharacter Char Char -- returns the expected char vs the actual char
  | MissingCharacters String -- returns the next chars that were expected
  | StringTooLong String -- returns the rest of the string
  | NoRule NT String -- [Maybe RuleChar]
    -- expects to parse some rule, returns top NT from stack and rest of string
  | UnexpectedCharacterWildCard [Char] Char Bool
  | MissingCharacterWildCard [Char] Bool
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
                        (True, False, NTChar k _) ->do next_level <- findRulesWithLevel (l-1) (k, Just c)
                                                       eps_rules <- findRulesWithLevel (l-1) (k,Nothing)
                                                       return (next_level ++ eps_rules /= [])
                        (True, _, _) -> return False
                        (False, _,_ )  -> return False

findRulesWithLevel l (i, Nothing)
  = do rules <- asks rules
       filterM helper rules
         where
           helper (_, Rule (j, s)) = if i == j then helper2 s else return False
           helper2 s = case s of
             [] -> return True
             ((NTChar k _):xs) -> do next_level <- findRulesWithLevel (l-1) (k, Nothing)
                                     if next_level /= [] then helper2 xs else return False
             _ -> return False


findRules :: (Monad m, MonadReader Grammar m, MonadError TableError m)
  => (NT, Maybe RuleChar) -> m [(Int, Rule)]
findRules (i,c) = do lmax <- asks (length . rules)
                     findRulesWithLevel lmax (i,c)



mkTable :: (Monad m, MonadError TableError m)
  => Grammar -> m Table
mkTable gr@(Grammar (ABC n ts wls bls) rules)
 = fromList <$> catMaybes <$>
  traverse helper [(i, c) | i <- [0..(n-1)] , c <-Nothing : map (Just . TChar) ts
                    ++ map (Just . WildCard True) wls
                    ++ map (Just . WildCard False) bls
                     ]
  where helper (i,c) = do rls <- runReaderT (findRules (i,c)) gr
                          case rls of [] -> return Nothing
                                      [x] -> return $ Just ((i,c), x)
                                      ls ->  throwError $ NonDeterministic ((i,c), map fst ls)

mkParseInfo :: (Monad m, MonadError TableError m)
  => Grammar -> m ParseInfo
mkParseInfo gr
  = do table <- mkTable gr
       let abc_parse = initToParse $ abc_init gr
       return $ ParseInfo table abc_parse


-----------------------parse------------------------------------


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

downFrom :: Int -> [Int]
downFrom 0 = []
downFrom n = [n, (n-1)..1]

chooseRule :: (Monad m, MonadReader ParseInfo m, MonadError ParseError m) =>
  NT -> String -> m (Int, Rule)
chooseRule i "" = do rule <- asks $ lookup (i, Nothing) . table
                     case rule of
                       Just r -> return r
                       Nothing -> throwError $ NoRule i ""
chooseRule i s = do (ABC n maxPrefixLength wls bls) <- asks abc_parse
                    let prefixes = map TChar $ map (\ l -> take l s) $ downFrom maxPrefixLength
                    let h = head s
                    let wlsFiltered = map (WildCard True) $ filter (h `elem`) wls
                    let blsFiltered = map (WildCard False) $ filter (not . (h `elem`)) bls
                    rule <- chooseRuleHelper i (map Just (prefixes ++ wlsFiltered ++ blsFiltered) ++ [Nothing])
                    case rule of
                      Just r -> return r
                      Nothing -> throwError $ NoRule i s
                                       
                       
parse :: (Monad m, MonadReader ParseInfo m, MonadError ParseError m) =>
  String -> m [ParseStep]
parse s = do (steps, rest) <- parseWithStack [NTChar 0 True] s
             case rest of "" -> return steps
                          _ -> throwError $ StringTooLong rest


changefst :: (a -> a) -> (a,b) -> (a,b)
changefst f (x,y) = (f x, y)

parseWithStack :: (Monad m, MonadReader ParseInfo m, MonadError ParseError m) =>
  [RuleChar] -> String -> m ([ParseStep], String)
parseWithStack [] s = return ([], s)
parseWithStack (TChar c : xs) s = parseWithStack xs =<< removePrefix c s
parseWithStack (NTChar i True : xs) s
  = do (k, r@(Rule (j, ls))) <- chooseRule i s
       changefst ((ParseRule k r) :) <$> parseWithStack (ls ++ xs) s
parseWithStack (NTChar i False : xs) s
  = do (substeps, s') <- parseWithStack [NTChar i True] s
       parseWithStack xs s'
parseWithStack (WildCard b cs : xs) "" = throwError $ MissingCharacterWildCard cs b
parseWithStack (WildCard b cs : xs) (c:s)
  = do case (c `elem` cs) == b of
         True -> changefst ((ParseChar c) :) <$> parseWithStack xs s
         False -> throwError $ UnexpectedCharacterWildCard cs c b

--splitAt, but returns error if i > length list
splitAtExcact :: Int -> [a] -> ([a], [a])
splitAtExcact i as = helper i [] as
  where helper 0 as bs = (reverse as, bs)
        helper i as (b:bs) = helper (i-1) (b:as) bs
        helper i as [] = error "split not possible"

mkTreeWithStack :: [(a,Int)] -> [Tree a] -> Tree a
mkTreeWithStack ((x, i): xs) trees = let (subforest, rest) = splitAtExcact i trees
                                         newNode = Node x subforest
                                     in mkTreeWithStack xs (newNode : rest)
mkTreeWithStack [] trees = case trees of [tree] -> tree
                                         [] -> error "BUG: This should not happen (empty tree)"
                                         _ -> error "BUG: This should not happen (too many trees)"

mkTree :: [(a,Int)] -> Tree a
mkTree rs = mkTreeWithStack (reverse rs) []


mkParseTree :: [ParseStep] -> ParseTree
mkParseTree = mkTree . map (\ step -> (step, countChildren step) )


