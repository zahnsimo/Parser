{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module MarkedString (stringToGrammar) where

import Control.Monad.State.Strict
import Data.List
import Data.List.Split

--import System.IO.Unsafe

import Grammar

data Marker = NTBracket Bool  -- for hidden NTs
            | RuleDivider -- seperates different rules 
            | Arrow -- seperates LHS and RHS of rules
            | WildCardBracket Bool -- true for WhiteList (c in list)
                                   -- false for BlackList (c notin list)
            | WildCardSeperator
  deriving (Eq, Show)

markerRepresentation :: Marker -> Char
markerRepresentation (NTBracket True) = '_'
markerRepresentation (NTBracket False) = '^'
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
markedStringToString = map markedCharToChar

charToMarkedChar :: Char -> MarkedChar
charToMarkedChar c = case c of
  '_' -> M (NTBracket True)
  '^' -> M (NTBracket False)
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
markedStringToRule (M (NTBracket b) : mcs) =
  do (lhs, rest) <- parseNT "" mcs b
     let rhs = parseArrow rest
     rulechars <- parse rhs
     return $ Rule (lhs, rulechars)
       where parse [] = return []
             parse (mc:mcs) = case mc of
               M (NTBracket b) -> do (j, rest) <- parseNT "" mcs b
                                     (NTChar j b:) <$> parse rest
               M RuleDivider -> error "unexpected RuleDivider"
               M Arrow -> error "unexpected arrow"
               M (WildCardBracket b) -> do let (list, rest) = parseWC [] mcs b
                                           (WildCard list b :) <$> parse rest
               C c -> do let (t, rest) = parseT [c] mcs
                         (TChar t :) <$> parse rest
             parseNT _ [] b = error "no closing NT bracket"               
             parseNT lebal (mc:mcs) b
               = case mc of C c -> parseNT (c : lebal) mcs b
                            M (NTBracket b) -> do names <- get
                                                  let label = reverse lebal
                                                  case elemIndex label names of
                                                    Just i -> return (i, mcs)
                                                    Nothing -> modify ( ++  [label]) >> return (length names, mcs)
                            _ -> error "unexpected symbol in NT label"
             parseArrow (mc:mcs)
               = case mc of M Arrow -> mcs
                            _ -> error "expected arrow"
             parseWC list (mc:mc':mcs) b = case (mc, mc') of
               (C c, M WildCardSeperator) -> parseWC (c : list) mcs b
               (C c , M (WildCardBracket b)) -> ((c:list), mcs)
               (_,_) -> error "wildcard list"
             parseWC list _ b = error "wildcard list"
             parseT str (mc:mcs) = case mc of
               C c -> parseT (c:str) mcs
               _ -> (reverse str, (mc:mcs))
             parseT str [] = (reverse str, [])
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
