module Ambiguity where

import LExp
import PySupport

operator_names :: String -> [(Var, PythonCode)] -> [Var]
operator_names operatorCode meanings = [ name | (name, code) <- meanings, words (text code) == words operatorCode]

flatten_operator :: [Var] -> LExp -> [LExp]
flatten_operator names (A (A (V x) e1) e2)
    | x `elem` names = flatten_operator names e1 ++ flatten_operator names e2
    | otherwise = [A (A (V x) e1) e2]

flatten_operator names e = [e]

same_expression :: LExp -> LExp -> Bool
same_expression (V x) (V y) = x == y

same_expression (A e11 e12) (A e21 e22) = same_expression e11 e21 && same_expression e12 e22

same_expression (L x e1) (L y e2) = x == y && same_expression e1 e2

same_expression _ _ = False

same_expressions :: [LExp] -> [LExp] -> Bool
same_expressions e1s e2s = length e1s == length e2s && and (zipWith same_expression e1s e2s)

same_meaning :: (Var, PythonCode) -> (Var, PythonCode) -> Bool
same_meaning (name1, code1) (name2, code2) = name1 == name 2 && text code1 == text code2

same_meanings :: [(Var, PythonCode)] -> [(Var, PythonCode)] -> Bool
same_meanings meanings1 meanings2 = length meanings1 == length meanings2 && and (zipWith same_meaning meanings1 meanings2)

