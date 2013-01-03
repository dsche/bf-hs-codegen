{-# LANGUAGE NoImplicitPrelude #-}
module BF where

import Prelude(Show(..),(++),concat,map,(.),($),String)

data Command = Plus | Minus | Left | Right | Out | In | Loop Program
instance Show Command where
 show Plus = "+"
 show Minus = "-"
 show Left = "<"
 show Right = ">"
 show Out = "."
 show In = ","
 show (Loop p) = "[" ++ (concat . (map show) $ p) ++ "]"

type Program = [Command]

print :: Program -> String
print = concat . (map show)

optimize :: Program -> Program 
optimize [] = []
optimize (Out:xs) = Out:(optimize xs)
optimize (In:xs) = In:(optimize xs)
optimize ((Loop p) : xs) = (Loop (optimize p)) : (optimize xs) 
optimize (Plus:xs) = plus (optimize xs) where
                      plus (Minus:ys) = ys
                      plus ys2 = Plus:ys2
optimize (Minus:xs) = minus (optimize xs) where
                       minus (Plus:ys) = ys
                       minus ys2 = Minus:ys2
optimize (Left:xs) = left (optimize xs) where
                      left (Right:ys) = ys
                      left ys2 = Left:ys2
optimize (Right:xs) = right (optimize xs) where
                       right (Left:ys) = ys
                       right ys2 = Right:ys2

