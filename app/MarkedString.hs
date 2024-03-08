{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module MarkedString (stringToGrammar) where

import Control.Monad.State.Strict
import Data.List
import Data.List.Split

--import System.IO.Unsafe

import Grammar

data Marker = NTBracket   -- Bool ; for hidden NTs
            | RuleDivider -- seperates different rules 
            | Arrow -- seperates LHS and RHS of rules
            | WildCardBracket Bool -- for BLs ad WLs
            | WildCardSeperator
  deriving (Eq, Show)

markerRepresentation :: Marker -> Char
markerRepresentation NTBracket = '_'
markerRepresentation RuleDivider = '$'
markerRepresentation Arrow = '~'
markerRepresentation (WildCardBracket True) = '!'
markerRepresentation (WildCardBracket False) = '@'
markerRepresentation WildCardSeperator = '&'

data MarkedChar = C Char | M Marker
  deriving (Eq, Show)

type MarkedString = [MarkedChar]

isChar :: MarkedChar -> Bool
isChar (C c) = True
isChar (M m) = False

isMarker :: MarkedChar -> Bool
isMarker = not . isChar

markedCharToChar :: MarkedChar -> Char
markedCharToChar mc = case mc of
  M m -> markerRepresentation m
  C c -> c

markedStringToString :: MarkedString -> String
markedStringToString cs = map markedCharToChar cs

charToMarkedChar :: Char -> MarkedChar
charToMarkedChar c = case c of
  '_' -> M NTBracket
  '$' -> M RuleDivider
  '~' -> M Arrow
  '!' -> M (WildCardBracket True)
  '@' -> M (WildCardBracket False)
  '&' -> M WildCardSeperator
  _   -> C c

stringToMarkedString :: String -> MarkedString
stringToMarkedString = map charToMarkedChar

markedStringToRule :: (Monad m, MonadState [String] m)
 => MarkedString -> m Rule
markedStringToRule (M NTBracket : ms) =
  do (lhs, rest) <- (parseNT "") ms
     let rhs = parseArrow rest
     rulechars <- parse rhs
     return $ Rule (lhs, rulechars)
       where parse :: (Monad m, MonadState [String] m)
               => MarkedString -> m [RuleChar]
             parse [] = return []
             parse (c:cs) = case c of
               M NTBracket -> do (j, rest) <- parseNT "" cs
                                 (NTChar j :) <$> parse rest
               M RuleDivider -> error "unexpected RuleDivider"
               M Arrow -> error "unexpected arrow"
               M (WildCardBracket b) -> do let (list, rest) = parseWC [] cs b
                                           case b of True  -> (BL list :) <$> parse rest
                                                     False -> (WL list :) <$> parse rest
               C char -> do let (t, rest) = parseT [char] cs
                            (TChar t :) <$> parse rest                            
             parseNT _ [] = error "no closing NT bracket"               
             parseNT label (c:cs)
               = case c of C x -> parseNT (label ++ [x]) cs
                           M NTBracket -> do names <- get
                                             case elemIndex label names of
                                               Just i -> return (i, cs)
                                               Nothing -> modify (\ ls -> ls ++  [label]) >> return (length names, cs)
                           _ -> error "unexpected symbol in NT label"
             parseArrow (c:cs)
               = case c of M Arrow -> cs
                           _ -> error "expected arrow"
             parseWC list (c:c':cs) b = case (c, c') of
               (C x, M WildCardSeperator) -> parseWC (x : list) cs b
               (C x , M (WildCardBracket b)) -> ((x:list), cs)
               (_,_) -> error "wildcard list"
             parseWC list _ b = error "wildcard list"
             parseT str (c:cs) = case c of
               C char -> parseT (str ++ [char]) cs
               _ -> (str, (c:cs))
             parseT str [] = (str, [])
markedStringToRule _ = error "expected NTBracket at start of rule"

markedStringToGrammar :: MarkedString -> Grammar
markedStringToGrammar ms = let split = splitOn [M RuleDivider] ms
                               (rules, labels) = runState (traverse markedStringToRule split) []
                               rulesWithInd = zip [0..] rules
                               n = length labels
                               ts = rmdups $ concatMap (getTs . getRHS) $ rules
                           in (Grammar n ts rulesWithInd)

stringToGrammar :: String -> Grammar
stringToGrammar = markedStringToGrammar . stringToMarkedString
                                    
ms = stringToMarkedString "_abc_~!g!"
ms2 = stringToMarkedString "_abc_~def_i_e"
