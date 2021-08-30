{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module DPLL (dpll, Literal, Clause, Formula, Assignment) where

import qualified Control.Applicative
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as Set

{- Represents a single variable, whose value can be either true or false -}
type Literal = Integer

{- Represents a disjunction of literals -}
type Clause = [Integer]

{- Represents a conjuction of clauses -}
type Formula = [Clause]

{- Represents a model, where positive literals have assigned truthy value and negative literals vice versa -}
type Assignment = [Literal]

{- Returns the first element of the list if the list is not empty -}
first :: [a] -> Maybe a
first [] = Nothing
first (x : _) = Just x

{- Returns the first symbol of the formula (which has unassigned value) -}
firstSymbol :: Formula -> Maybe Literal
firstSymbol formula = do
  a <- first formula
  first a

{- Make the list contain only unique values, removing all repetitions -}
uniq :: Ord a => [a] -> [a]
uniq = Set.toList . Set.fromList

{- Returns whether the formula contains empty clause, in which case the formula contains contradiction
and as a result is is unsatisfiable -}
containsEmptyClause :: Formula -> Bool
containsEmptyClause = any null

{- Returns pure literal from the formula (if it exits). Pure literal is a literal,
  which has same sign across all clauses. If it exists, it can be safely assigned value
  and clauses containing it can be removed. -}
findPureLiteral :: Formula -> Maybe Literal
findPureLiteral formula = first pureLiteral
  where
    allFormulaLiterals = uniq $ concat formula
    negativeLiteralNotPresent x = (- x) `notElem` allFormulaLiterals
    pureLiteral = filter negativeLiteralNotPresent allFormulaLiterals

{- Returns a literal, which is in an unit clause.
  Unit clause is a clause containing just single literal, which value can be set safely. -}
findUnitClauseLiteral :: Formula -> Maybe Literal
findUnitClauseLiteral = foldr step Nothing
  where
    step [a] _ = Just a
    step _ acc = acc

{- Returns a formula with assigned value to literal. If a literal in in clause with the same sign,
  the whole clause can be removed from the formula, otherwise just the literal -}
assignLiteralValue :: Literal -> Formula -> Formula
assignLiteralValue literal = mapMaybe mapClause
  where
    mapClause clause
      | literal `elem` clause = Nothing
      | otherwise = Just $ filter (/= - literal) clause

{- Main DPLL algorithm, finding a solution for the instance of the SAT problem -}
dpll' :: Assignment -> Formula -> Maybe Assignment
dpll' assignment formula
  {- Base cases -}

  {- If the formula is empty, then all the clauses are satisfied and the solution is found -}
  | null formula = Just assignment
  {- If the formula contains the empty clause, it contains a contradiction and thus is unsatisfiable -}
  | containsEmptyClause formula = Nothing
  {- Heuristic shortcuts -}
  {- If the formula contains pure literal, this literal can be assigned
    a value safely without the loss of generality, clauses which will be satisfied by this literal can be removed
    and literals of the negated value can be removed -}
  | isJust pureLiteral =
    let Just symbol = pureLiteral
     in dpll' (symbol : assignment) (assignLiteralValue symbol formula)
  {- If the formula contains unit clause, it can be satisfied only in one way and we can assign a value to a literal -}
  | isJust unitClauseLiteral =
    let Just symbol = unitClauseLiteral
     in dpll' (symbol : assignment) (assignLiteralValue symbol formula)
  {- We are performing a backtracking operation, where we assign true and false value to a literal -}
  | otherwise = dpllBacktrack' assignment formula
  where
    pureLiteral = findPureLiteral formula
    unitClauseLiteral = findUnitClauseLiteral formula

{- Backtracking part of the DPLL algorithm, where we calculate the result
  for assigning true and false value to a literal -}
dpllBacktrack' :: Assignment -> Formula -> Maybe Assignment
dpllBacktrack' assignment formula = do
  symbol <- firstSymbol formula
  let literalResult = dpll' (symbol : assignment) (assignLiteralValue symbol formula)
  let negatedLiteralResult = dpll' ((- symbol) : assignment) (assignLiteralValue (- symbol) formula)
  literalResult Control.Applicative.<|> negatedLiteralResult

{-   case literalResult of
    Just result -> Just result
    Nothing -> negatedLiteralResult -}

{- Exported member, providing only the place for the formula assignment -}
dpll :: Formula -> Maybe Assignment
dpll = dpll' []
