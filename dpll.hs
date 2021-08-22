{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module DPLL where

import Data.Maybe (isJust, mapMaybe)
import Data.Set (fromList, toList)

type Literal = Integer

type Clause = [Integer]

type Formula = [Clause]

type Assignment = [Literal]

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

uniq :: Ord a => [a] -> [a]
uniq list = toList $ fromList list

containsEmptyClause :: Formula -> Bool
containsEmptyClause = any isEmpty

findPureSymbol :: (Ord a, Foldable t, Num a) => t [a] -> Maybe a
findPureSymbol formula = case pureSymbols of
  [] -> Nothing
  (a : _) -> Just a
  where
    symbolUnion = uniq $ concat formula
    negativeLiteralNotPresent x = (- x) `notElem` symbolUnion
    pureSymbols = filter negativeLiteralNotPresent symbolUnion

findUnitClauseSymbol :: [[a]] -> Maybe a
findUnitClauseSymbol = foldr step Nothing
  where
    step [a] _ = Just a
    step _ acc = acc

setSymbolInFormula :: (Eq a, Num a) => a -> [[a]] -> [[a]]
setSymbolInFormula symbol = mapMaybe mapFn
  where
    mapFn clause
      | symbol `elem` clause = Nothing
      | otherwise = Just $ filter (/= - symbol) clause

dpll' :: (Assignment -> Formula -> Maybe Assignment)
dpll' assignment [] = Just assignment
dpll' assignment formula = case (containsEmptyClause formula, pureSymbol, unitClauseSymbol) of
  (True, _, _) -> Nothing
  (_, Just symbol, _) -> dpll' (symbol : assignment) (setSymbolInFormula symbol formula)
  (_, _, Just symbol) -> dpll' (symbol : assignment) (setSymbolInFormula symbol formula)
  (_, _, _) -> dpllBacktrack' assignment formula
  where
    pureSymbol = findPureSymbol formula
    unitClauseSymbol = findUnitClauseSymbol formula

firstSymbol :: [[a]] -> Maybe a
firstSymbol ((x : _) : _) = Just x
firstSymbol _ = Nothing

dpllBacktrack' :: Assignment -> Formula -> Maybe Assignment
dpllBacktrack' assignment formula = do
  symbol <- firstSymbol formula
  let positiveSymbolResult = dpll' (symbol : assignment) (setSymbolInFormula symbol formula)
  let negativeSymbolResult = dpll' ((- symbol) : assignment) (setSymbolInFormula (- symbol) formula)
  let returnValue = if isJust positiveSymbolResult then positiveSymbolResult else negativeSymbolResult
  returnValue

dpll :: Formula -> Maybe Assignment
dpll = dpll' []