{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import DPLL (Assignment, Clause, dpll)
import Data.List (intercalate)

main :: IO ()
main = do
  content <- getContents
  let contentLines = lines content
  let model = dpll $ map convertLine contentLines
  putStrLn $ modelToString model
  where
    modelToString :: (Maybe Assignment -> String)
    modelToString Nothing = "No model exists"
    modelToString (Just assignment) =
      "Found model: "
        ++ intercalate ", " (map show assignment)

    convertLine :: String -> Clause
    convertLine = map read . words
