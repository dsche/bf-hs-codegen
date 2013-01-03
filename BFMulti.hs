{-# LANGUAGE NoImplicitPrelude #-}
module BFMulti where

import Prelude(Int, maximum, (.), map, ($), (++), Ordering(..), compare, (*), (-), replicate, concat, (+), (*), foldr1, flip, elem, length, (==), Bool(..), Show(..), String )
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

compile p = let n = last_tape p in compile' n p 

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
 

compile' n p = BF.optimize ( init ++ (concat . (map compile_single) $ p)) where     
     init = concat . (replicate (n+1)) $ [BF.Plus, BF.ShiftR, BF.ShiftR]
     compile_single (Plus i) = searchAndModify n i [BF.Plus]
     compile_single (Minus i) = searchAndModify n i [BF.Minus]
     compile_single (In i) = searchAndModify n i [BF.In]
     compile_single (Out i) = searchAndModify n i [BF.Out]
     compile_single (ShiftL i) = searchAndModify n i [BF.ShiftL, BF.Minus, BF.ShiftR]
     compile_single (ShiftR i) = searchAndModify n i ((replicate (2*(n+1) - 1) BF.ShiftR) ++ [BF.Plus, BF.ShiftR])
     compile_single (Loop i p') = (search n i) ++ [BF.Loop ((jump (-2*i-1)) ++ (compile' n p') ++ (search n i))] ++ (jump (-2*i-1)) 
     

search :: Int -> Int -> BF.Program
search n i =           (jump (2*i)) 
                     ++ [BF.Loop (jump (2*(n+1)))]
                     ++ [BF.Minus]
                     ++ [BF.Loop ([BF.Plus]++(jump (-2*(n+1)))++[BF.Minus])]
                     ++ [BF.Plus]
                     ++ (jump (2*(n+1)))
                     ++ [BF.ShiftR]
      
searchAndModify :: Int -> Int -> BF.Program -> BF.Program
searchAndModify n i c = (search n i) 
                      ++ c
                      ++ (jump (-2*i-1))    

jump :: Int -> BF.Program     
jump n = case n `compare` 0 of
     LT -> replicate (-n) BF.ShiftL
     EQ -> []
     GT -> replicate n BF.ShiftR


