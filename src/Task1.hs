{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where
import Data.Maybe (fromJust, isJust) 

-- * Expression data type

-- | Representation of integer arithmetic expressions comprising
-- - Literals of type 'a'
-- - Binary operations 'Add' and 'Mul'
data IExpr =
    Lit Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  deriving Show

-- * Evaluation

-- | Evaluates given 'IExpr'
--
-- Usage example:
--
-- >>> evalIExpr (Lit 2)
-- 2
-- >>> evalIExpr (Add (Lit 2) (Lit 3))
-- 5
-- >>> evalIExpr (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- 9
--
evalIExpr :: IExpr -> Integer
evalIExpr (Lit n) = n
evalIExpr (Add e1 e2) = evalIExpr e1 + evalIExpr e2
evalIExpr (Mul e1 e2) = evalIExpr e1 * evalIExpr e2

-- * Parsing

-- | Class of parseable types
class Parse a where
  -- | Parses value 'a' from given string
  -- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
  parse :: String -> Maybe a


instance Parse Integer where
  parse s = case reads s of
    [(n, "")] -> Just n
    _ -> Nothing
-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe IExpr
-- Just (Lit 2)
-- >>> parse "2 3 +" :: Maybe IExpr
-- Just (Add (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe IExpr
-- Just (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe IExpr
-- Nothing
-- >>> parse "2 3" :: Maybe IExpr
-- Nothing
--

instance Parse IExpr where
  parse expr = parseHelper (words expr) where 
    parseHelper :: [String] -> Maybe IExpr
    parseHelper = go []  where
      go [result] [] = result
      go _ [] = Nothing
      go acc (x:xs) = case x of
        _ | all (`elem` ['0'..'9']) x -> go (acc ++ [Just (Lit (read x))]) xs
        "+" -> if isJust (last acc) && isJust (last (init acc)) then go (take (length acc - 2) acc ++ [Just (Add (fromJust (last acc)) (fromJust (last (init acc))))]) xs else Nothing
        "*" -> if isJust (last acc) && isJust (last (init acc)) then go (take (length acc - 2) acc ++ [Just (Mul (fromJust (last acc)) (fromJust (last (init acc))))]) xs else Nothing
        _ -> Nothing


-- * Evaluation with parsing

-- | Parses given expression in Reverse Polish Notation and evaluates it
--
-- Returns 'Nothing' in case the expression could not be parsed.
--
-- Usage example:
--
-- >>> evaluateIExpr "2"
-- Just 2
-- >>> evaluateIExpr "2 3 +"
-- Just 5
-- >>> evaluateIExpr "3 2 * 3 +"
-- Just 9
-- >>> evaluateIExpr "2 +"
-- Nothing
-- >>> evaluateIExpr "2 3"
-- Nothing
--
evaluateIExpr :: String -> Maybe Integer
evaluateIExpr expr = case parsed of
    Nothing -> Nothing
    Just (Lit n) -> Just n
    Just (Add e1 e2) -> Just (simplify e1 + simplify e2)
    Just (Mul e1 e2) -> Just (simplify e1 * simplify e2)
  where
    parsed = parse expr
    simplify :: IExpr -> Integer 
    simplify (Lit n) = n
    simplify (Add e1 e2) = simplify e1 + simplify e2
    simplify (Mul e1 e2) = simplify e1 * simplify e2

      
