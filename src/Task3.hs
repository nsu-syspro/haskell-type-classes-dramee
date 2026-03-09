{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Task2 (evaluate, BoolOp)
import Data.List (nub)

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
--



evaluateBool :: [(String, Bool)] -> String -> Maybe Bool
evaluateBool = evaluate @_ @BoolOp

getVars :: String -> [String]
getVars = filter (/= "and") . filter (/= "or") . filter (/= "xor") . words

solveSAT :: String -> Maybe Bool
solveSAT str =
  case evaluateBool testAssign str of
    Nothing -> Nothing
    Just _  -> Just (any (\a -> evaluateBool a str == Just True) assignments)
  where
    vars = nub (getVars str)

    testAssign = zip vars (repeat False)

    assignments = map (zip vars) (boolComb (length vars))

    boolComb :: Int -> [[Bool]]
    boolComb 0 = [[]]
    boolComb n = [b:bs | b <- [True, False], bs <- boolComb (n-1)]