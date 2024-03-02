{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

import Control.Monad.State.Strict
import Data.List
import Data.List.Split

import System.IO.Unsafe

import Grammar

data Marker = NTBracket -- Bool ; for hidden NTs
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
data MarkedString = MS [MarkedChar]
  deriving Show

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
markedStringToString (MS cs) = map markedCharToChar cs

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
stringToMarkedString = MS . (map charToMarkedChar)

markedStringToRule :: (Monad m, MonadState [String] m)
 => MarkedString -> m Rule
markedStringToRule (MS ms) =
  do (lhs, rest) <- parseNT $ tail ms
     let rhs = parseArrow rest
     -- unsafePerformIO (do print lhs
     --                     print rhs
     --                     return undefined)
     rulechars <- parse rhs
     return $ Rule (lhs, rulechars)
       where parseNT cs
               = do let (ntchars, rest) = break (== M NTBracket) cs
                    if | rest == [] -> error "expected snd NTbracket" 
                       | all isChar ntchars -> do let ntname = map (\ (C c) -> c) ntchars
                                                  names <- get
                                                  case elemIndex ntname names of
                                                    Just i -> return (i, tail rest)
                                                    Nothing -> modify (\ ls -> ls ++  [ntname]) >> return (length names, tail rest)
                       | otherwise -> error "only non special chars between NTbrackets"

             parseArrow (c:cs)
               = case c of M Arrow -> cs
                           _ -> error "expected arrow"
             parse [] = return []
             parse (c:cs) = case c of
               M NTBracket -> do (j, rest) <- parseNT cs
                                 prest <- parse rest
                                 return (NTChar j : prest)
               M RuleDivider -> error "no RuleDivider within rules"
               M Arrow -> error "too many arrows"
               M (WildCardBracket b) -> do let (list, rest) = parseWC [] cs b
                                           prest <- parse rest
                                           case b of True -> return (BL list : prest)
                                                     False -> return (WL list : prest)
               C char -> do let (t, rest) = parseT [char] cs
                            prest <- parse rest
                            return (TChar t : prest)
             parseWC list (c:c':cs) b = case (c, c') of
               (C x, M WildCardSeperator) -> parseWC (x : list) cs b
               (C x , M (WildCardBracket b)) -> ((x:list), cs)
               (_,_) -> error "wildcard list"
             parseWC list _ b = error "wildcard list"
             parseT str (c:cs) = case c of
               C char -> parseT (str ++ [char]) cs
               _ -> (str, (c:cs))
             parseT str [] = (str, [])
  --let (lhs, rhs) = (\ (a,b) -> (a, parseArrow b) ) $ parseNT ms
  

markedStringToGrammar :: MarkedString -> Grammar
markedStringToGrammar (MS ms) = let split = map MS $ splitOn [M RuleDivider] ms
                                    (rules, labels) = runState (traverse markedStringToRule split) []
                                    rulesWithInd = zip [1..] rules
                                    n = length labels
                                    ts = rmdups $ concatMap (getTs . getRHS . snd) $ rulesWithInd
                                 in (Grammar n ts rulesWithInd)
                                    
ms = stringToMarkedString "_abc_~!g!"
ms2 = stringToMarkedString "_abc_~def_i_e"
