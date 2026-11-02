{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lambek where

import LTypes

my_tails :: [a] -> [[a]]
my_tails = foldr (\x result -> (x : (head result)) : result) [[]]

my_heads :: [a] -> [[a]]
my_heads = foldr (\x result -> ([]:(map (x :) result))) [[]] 

splits :: [a] -> [([a], [a])]
splits xs = zip (my_heads xs) (my_tails xs)

{-

type Atm = String
data LTp = Atm Atm | DivL LTp LTp | DivR LTp LTp

The above come from LTypes hence we can remove them

Notice that DivR a b is really A / B

a possible implementation of the list or

or:: [Bool] -> Bool
or = foldr (||) False

null []    = True
null (_:_) = False

-}

dec_rinv :: [LTp] -> LTp -> Bool

dec_rinv gamma (DivL a b) = dec_rinv (a:gamma) b

dec_rinv gamma (DivR a b) = dec_rinv (gamma ++ [b]) a

dec_rinv gamma (Atm p) = or [dec_lfoc gammaL a gammaR p | (gammaL, a: gammaR ) <- splits gamma]


dec_lfoc :: [LTp] -> LTp -> [LTp] -> Atm -> Bool

dec_lfoc gammaL (DivL a b) gammaR p =  or [(dec_rinv gammaL2 a) && (dec_lfoc gammaL1 b gammaR p) |
                                            (gammaL1, gammaL2) <- splits gammaL]

dec_lfoc gammaL (DivR a b) gammaR p =  or [(dec_rinv gammaR1 b) && (dec_lfoc gammaL a gammaR2 p) |
                                            (gammaR1, gammaR2) <- splits gammaR]

dec_lfoc gammaL (Atm a) gammaR p = null gammaL && null gammaR && a == p

possible_types :: Lexicon sem -> String -> [LTp]
possible_types lex w = [a | (word, a, v) <- (items lex), word == w]

isGrammatical_helper :: Lexicon sem -> LTp -> [String] -> Bool
isGrammatical_helper lex p [] = dec_rinv [] p
isGrammatical_helper lex p (w:ws) = or [isGrammatical_helper lex (DivL a p) ws | a <- possible_types lex w] 

isGrammatical :: Lexicon sem -> [String] -> Bool
isGrammatical lex w = isGrammatical_helper lex (stp lex) w

