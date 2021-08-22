{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module DPLL where

import Data.List

type Literal = Integer

type Clause = [Integer]

type Formula = [Clause]

type Assignment = [Literal]

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

containsEmptyClause :: Formula -> Bool
containsEmptyClause = any isEmpty

findPureSymbol :: (Foldable t, Eq a, Num a) => t [a] -> Maybe a
findPureSymbol formula = case pureSymbols of
  [] -> Nothing
  (a : _) -> Just a
  where
    symbolUnion = foldr1 union formula
    negativeLiteralNotPresent x = (- x) `notElem` symbolUnion
    pureSymbols = filter negativeLiteralNotPresent symbolUnion

findUnitClauseSymbol :: [[a]] -> Maybe a
findUnitClauseSymbol = foldr step Nothing
  where
    step [a] _ = Just a
    step _ acc = acc

setSymbolInFormula :: Literal -> Formula -> Formula
setSymbolInFormula symbol formula = error "to implement"
  where
    step value acc = acc

dpll :: (Assignment -> Formula -> Maybe Assignment)
dpll assignment [] = Just assignment
dpll assignment formula = case (containsEmptyClause formula, pureSymbol, unitClauseSymbol) of
  (True, _, _) -> Nothing
  (_, Just symbol, _) -> Nothing
  (_, _, Just symbol) -> Nothing
  (_, _, _) -> dpllBacktrack assignment formula
  where
    pureSymbol = findPureSymbol formula
    unitClauseSymbol = findUnitClauseSymbol formula

dpllBacktrack assignment formula = error "to Implement"