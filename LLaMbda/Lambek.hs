{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lambek where

import LTypes
import LExp

my_tails :: [a] -> [[a]]
my_tails = foldr (\x result -> (x : (head result)) : result) [[]]

my_heads :: [a] -> [[a]]
my_heads = foldr (\x result -> ([]:(map (x :) result))) [[]] 

splits :: [a] -> [([a], [a])]
splits xs = zip (my_heads xs) (my_tails xs)

-- splits:: [a] -> [([a], [a])]
-- splits xs =
--     [splitAt i xs | i <- [0 .. length xs]]

dec_rinv :: [LTp] -> LTp -> Bool

dec_rinv gamma (DivL a b) = dec_rinv (a:gamma) b
dec_rinv gamma (DivR a b) = dec_rinv (gamma ++ [b]) a
dec_rinv gamma (Atm p) = or [dec_lfoc gammaL a gammaR p | (gammaL, a: gammaR ) <- splits gamma]


dec_lfoc :: [LTp] -> LTp -> [LTp] -> Atm -> Bool

dec_lfoc gammaL (DivL a b) gammaR p =  or [(dec_rinv gammaL2 a) && (dec_lfoc gammaL1 b gammaR p) |
                                            (gammaL1, gammaL2) <- splits gammaL]

dec_lfoc gammaL (DivR a b) gammaR p =  or [(dec_rinv gammaR1 b) && (dec_lfoc gammaL a gammaR2 p) |
                                            (gammaR1, gammaR2) <- splits gammaR]

dec_lfoc gammaL (Atm q) gammaR p = null gammaL && null gammaR && q == p

possible_types :: Lexicon sem -> String -> [LTp]
possible_types lex w = [a | (word, a, v) <- (items lex), word == w]

isGrammatical_helper :: Lexicon sem -> LTp -> [String] -> Bool
isGrammatical_helper lex p [] = dec_rinv [] p
isGrammatical_helper lex p (w:ws) = or [isGrammatical_helper lex (DivL a p) ws | a <- possible_types lex w] 

isGrammatical :: Lexicon sem -> [String] -> Bool
isGrammatical lex ws = isGrammatical_helper lex (stp lex) ws



-- Transforming sequent calculus derivations into lambda expressions

der_rinv :: [(Var,LTp)] -> LTp -> [LExp]

der_rinv gamma (DivL a b) = [L x e | e <- der_rinv ((x, a):gamma) b] where x = fresh [V y | (y, _) <- gamma]
der_rinv gamma (DivR a b) = [L x e | e <- der_rinv (gamma ++ [(x, b)]) a] where x = fresh [V y | (y, _) <- gamma]
der_rinv gamma (Atm p) = [ foldl (\x -> A x) (V x) args| (gammaL, (x,a): gammaR ) <- splits gamma, args <- der_lfoc gammaL a gammaR p]

der_lfoc :: [(Var,LTp)] -> LTp -> [(Var,LTp)] -> Atm -> [[LExp]]


der_lfoc gammaL (DivL a b) gammaR p = [e : args | (gammaL1, gammaL2) <- splits(gammaL), e <- der_rinv gammaL2 a, args <- der_lfoc gammaL1 b gammaR p]
der_lfoc gammaL (DivR a b) gammaR p = [e : args | (gammaR1, gammaR2) <- splits(gammaR), args <- der_lfoc gammaL a gammaR2 p, e <- der_rinv gammaR1 b]

der_lfoc gammaL (Atm q) gammaR p = if null gammaL && null gammaR && q == p 
                                    then [[]]
                                    else []


derivations_helper :: Lexicon sem -> LTp -> [String] -> Int -> [(LExp, [(Var, sem)])]
derivations_helper lex p [] i = [(e,[]) | e <- der_rinv [] p ]
derivations_helper lex p (w:ws) i = [(expression, meanings) | (word, a, word_meaning) <- items lex, word == w,  (e, rest_meanings) <- derivations_helper lex (DivL a p) ws (i+1),
                                        let expression = normalize (A e (V name)), let meanings = (name, word_meaning): rest_meanings] 
                                        where name = w ++ "_" ++ show i


derivations :: Lexicon sem -> [String] -> [(LExp, [(Var, sem)])]
derivations lex ws = derivations_helper lex (stp lex) ws 0

new_derivations :: Lexicon sem -> [String] -> [(LExp, [(Var, sem)])]
new_derivations lex ws = derivations lex ws ++ derivations_helper lex (Atm "q") ws 0

