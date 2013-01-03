{-# LANGUAGE NoImplicitPrelude #-}
module BFMulti where

import Prelude(Int, maximum, (.), map, ($), (++), Ordering(..), compare, (*), (-), replicate, concat, (+), (*), nub)
import qualified BF

data Command = Plus Int | Minus Int | Left Int | Right Int | Out Int | In Int | Loop Int Program

type Program = [Command]

optimize :: Program -> Program
optimize p =  foldr1 (.) (map ((flip optimize_step) []) [0..(last_tape p)]) $ (map (\x -> [x]) p) where

-- index, accumulator, code, result
optimize_step :: Int -> Program -> [Program] -> [Program]
optimize_step i acc [] = acc
optimize_step i acc (x:xs) = case (i `elem` (uses x), length (uses x) == 1) of
                              (True,True) -> optimize_step i (acc ++ x) xs
                              (True,False) -> (acc ++ x): (optimize_step i [] xs)
                              (False,_) -> x:(optimize_step i acc xs)  

uses :: Command -> [Int]
uses (Plus k) = [k]
uses (Minus k) = [k]
uses (Left k) = [k]
uses (Right k) = [k]
uses (Out k) = [k]
uses (In k) = [k]
uses (Loop k p) = nub ([k] ++ (concat . (map uses)  $ p))     

compile :: Program -> BF.Program

compile p = let n = num_tapes p in compile' n p where
     index (Plus k) = k
     index (Minus k) = k
     index (Left k) = k
     index (Right k) = k
     index (Out k) = k
     index (In k) = k
     index (Loop k _) = k

last_tape :: Program -> Int
last_tape = maximum . (map index) 

compile' n p = BF.optimize ( init ++ (concat . (map compile_single) $ p)) where     
     init = concat . (replicate (n+1)) $ [BF.Plus, BF.Right, BF.Right]
     compile_single (Plus i) = searchAndModify n i [BF.Plus]
     compile_single (Minus i) = searchAndModify n i [BF.Minus]
     compile_single (In i) = searchAndModify n i [BF.In]
     compile_single (Out i) = searchAndModify n i [BF.Out]
     compile_single (Left i) = searchAndModify n i [BF.Left, BF.Minus, BF.Right]
     compile_single (Right i) = searchAndModify n i ((replicate (2*(n+1) - 1) BF.Right) ++ [BF.Plus, BF.Right])
     compile_single (Loop i p') = (search n i) ++ [BF.Loop ((jump (-2*i-1)) ++ (compile' n p') ++ (search n i))] ++ (jump (-2*i-1)) 
     

search :: Int -> Int -> BF.Program
search n i =           (jump (2*i)) 
                     ++ [BF.Loop (jump (2*(n+1)))]
                     ++ [BF.Minus]
                     ++ [BF.Loop ([BF.Plus]++(jump (-2*(n+1)))++[BF.Minus])]
                     ++ [BF.Plus]
                     ++ (jump (2*(n+1)))
                     ++ [BF.Right]
      
searchAndModify :: Int -> Int -> BF.Program -> BF.Program
searchAndModify n i c = (search n i) 
                      ++ c
                      ++ (jump (-2*i-1))    

jump :: Int -> BF.Program     
jump n = case n `compare` 0 of
     LT -> replicate (-n) BF.Left
     EQ -> []
     GT -> replicate n BF.Right


