{-# LANGUAGE NoImplicitPrelude #-}
module BFMulti where

import Prelude(Int, maximum, (.), map, ($), (++), Ordering(..), compare, (*), (-), replicate, concat, (+), (*), foldr1, flip, elem, length, (==), Bool(..), Show(..), String, abs, (>) )
import Data.List(nub)
import qualified BF

data Command = Plus Int | Minus Int | ShiftL Int | ShiftR Int | Out Int | In Int | Loop Int Program
instance Show Command where
 show (Plus k) = "+"++(show k)
 show (Minus k) = "-"++(show k)
 show (ShiftL k) = "<"++(show k)
 show (ShiftR k) = ">"++(show k)
 show (Out k) = "."++(show k)
 show (In k) = ","++(show k)
 show (Loop k p) = "["  ++ (show k) ++ " " ++ (print p) ++ "]"
 
print :: Program -> String
print (c:cs) = (show c) ++ (concat . map (\x -> " " ++ (show x)) $ cs)
 
type Program = [Command]

optimize :: Program -> Program
optimize p =  concat $ foldr1 (.) (map ((flip optimize_step) []) [0..(last_tape p)]) $ (map (\x -> [x]) p) where

-- index, accumulator, code, result
optimize_step :: Int -> Program -> [Program] -> [Program]
optimize_step i acc [] = [acc]
optimize_step i acc (x:xs) = case (i `elem` (uses x), length (uses x) == 1) of
                              (True,True) -> optimize_step i (acc ++ x) xs
                              (True,False) -> (acc ++ x): (optimize_step i [] xs)
                              (False,_) -> x:(optimize_step i acc xs)  


uses :: Program -> [Int]
uses = concat . (map command_uses)

command_uses :: Command -> [Int]
command_uses (Loop k p) = nub ([k] ++ (uses p))     
command_uses c = [index c]

compile :: Program -> BF.Program
compile p = let n = last_tape p in BF.optimize $ compile' n (optimize p) 

last_tape :: Program -> Int
last_tape = maximum . (map index) 

index :: Command -> Int
index (Plus k) = k
index (Minus k) = k
index (ShiftL k) = k
index (ShiftR k) = k
index (Out k) = k
index (In k) = k
index (Loop k _) = k
 


compile' :: Int -> Program -> BF.Program
compile' _ [] = []
compile' n (p:ps) = init ++ (jump (2*(index p) + 1)) ++ (perform p) ++ (compile'' (index p) ps) where
                      init = concat . (replicate (n+1)) $ [BF.Plus, BF.ShiftR, BF.ShiftR]
                      
                      perform (Plus _) = [BF.Plus]
                      perform (Minus _) = [BF.Minus]
                      perform (In _) = [BF.In]
                      perform (Out _) = [BF.Out]
                      perform (ShiftL _) = jump (-2*(n+1)) ++ [BF.ShiftL] ++ [BF.Minus] ++ [BF.ShiftR]
                      perform (ShiftR _) = [BF.ShiftL] ++ [BF.Plus] ++ [BF.ShiftR] ++ jump (2*(n+1))
                      perform (Loop i p) = [BF.Loop (compile'' i (p ++ [Plus i, Minus i]))]  --  <--- HACKY
                      
                      compile'' :: Int -> Program -> BF.Program
                      compile'' i [] = []
                      compile'' i (p:ps) = case (index p == i, abs (2*(index p - i)-1) > n+1, 2*(index p - i)-1 > 0 ) of
                                   (True,_,_) -> perform p ++ compile'' i ps
                                   (False,False,_) -> jump (2*(index p - i)-1) ++ search ++ [BF.ShiftR] ++ perform p ++ compile'' (index p) ps
                                   (False,True,True) -> jump (2*(index p - i)-1 - 2*(n+1)) ++ search ++ [BF.ShiftR] ++ perform p ++ compile'' (index p) ps  
                                   (False,True,False) -> jump (2*(index p - i)-1 + 2*(n+1)) ++ search ++ [BF.ShiftR] ++ perform p ++ compile'' (index p) ps
                      
                      search = [BF.Loop (jump (2*(n+1)))] ++ [BF.Minus] ++ [BF.Loop ([BF.Plus]++(jump (-2*(n+1)))++[BF.Minus])] ++ [BF.Plus]
     

jump :: Int -> BF.Program     
jump n = case n `compare` 0 of
     LT -> replicate (-n) BF.ShiftL
     EQ -> []
     GT -> replicate n BF.ShiftR


