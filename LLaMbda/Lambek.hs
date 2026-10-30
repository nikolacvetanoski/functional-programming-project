{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lambek where

import LTypes

my_tails :: [a] -> [[a]]
my_tails = foldr (\x result -> (x : (head result)) : result) [[]]

my_heads :: [a] -> [[a]]
my_heads = foldr (\x result -> ([]:(map (x :) result))) [[]] 


splits :: [a] -> [([a], [a])]
splits xs = zip (my_heads xs) (my_tails xs)

dec_rinv :: [LTp] -> LTp -> Bool
dec_rinv gamma b = undefined

dec_lfoc :: [LTp] -> LTp -> [LTp] -> Atm -> Bool
dec_lfoc gammaL a gammaR p = undefined

isGrammatical :: Lexicon sem -> [String] -> Bool
isGrammatical lex ws = undefined

