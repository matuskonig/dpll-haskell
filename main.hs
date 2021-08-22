{-# LANGUAGE ScopedTypeVariables #-}

import DPLL (Assignment, Formula, Literal, dpll)
import Data.List (intercalate)

main :: IO ()
main = do
  content <- getContents
  let contentLines = lines content
  let model = dpll $ map read contentLines
  putStrLn $ modelToString model
  where
    modelToString :: (Maybe Assignment -> String)
    modelToString Nothing = "No model found"
    modelToString (Just assignment) = intercalate ", " $ map show assignment