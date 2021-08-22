{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import DPLL (Assignment, Clause, dpll)
import Data.List (intercalate)

noModelFoundMessage :: String
noModelFoundMessage = "No model exists"

main :: IO ()
main = do
  content <- getContents
  let model = dpll $ map convertLine $ lines content
  putStrLn $ modelToString model
  where
    {- If the model is found, return a string concatenating the model literals,
      else return errr message  -}
    modelToString :: (Maybe Assignment -> String)
    modelToString Nothing = noModelFoundMessage
    modelToString (Just assignment) =
      "Found model: "
        ++ intercalate ", " (map show assignment)

    {- Converts single input line into clause -}
    convertLine :: (String -> Clause)
    convertLine = map read . words
