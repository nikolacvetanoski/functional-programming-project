module Ambiguity where

import LExp
import PySupport

operator_names :: String -> [(Var, PythonCode)] -> [Var]
operator_names operator_code meanings = [ name | (name, code) <- meanings, words (text code) == words operator_code]

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
same_meaning (name1, code1) (name2, code2) = name1 == name2 && text code1 == text code2

same_meanings :: [(Var, PythonCode)] -> [(Var, PythonCode)] -> Bool
same_meanings meanings1 meanings2 = length meanings1 == length meanings2 && and (zipWith same_meaning meanings1 meanings2)

same_flattening :: String -> [(Var, PythonCode)] -> LExp -> LExp -> Bool
same_flattening operator_code meanings e1 e2 = same_expressions (flatten_operator names e1) (flatten_operator names e2)
                                                where names = operator_names operator_code meanings

equivalent_derivations :: (LExp, [(Var, PythonCode)]) -> (LExp, [(Var, PythonCode)]) -> Bool
equivalent_derivations (e1, meanings1) (e2, meanings2) = same_meanings meanings1 meanings2 && (same_and || same_or) 
                                                            where 
                                                                same_and = same_flattening "lambda x : lambda y : x and y" meanings1 e1 e2
                                                                same_or = same_flattening "lambda x : lambda y : x or y" meanings1 e1 e2

unique_derivations :: [(LExp, [(Var, PythonCode)])] -> [(LExp, [(Var, PythonCode)])] 
unique_derivations [] = []
unique_derivations (d:ds) = d: unique_derivations [e | e <- ds, not (equivalent_derivations d e)]