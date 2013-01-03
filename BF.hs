{-# LANGUAGE NoImplicitPrelude #-}
module BF where

import Prelude(Show(..),(++),concat,map,(.),($),String)

data Command = Plus | Minus | ShiftL | ShiftR | Out | In | Loop Program
instance Show Command where
 show Plus = "+"
 show Minus = "-"
 show ShiftL = "<"
 show ShiftR = ">"
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
optimize (ShiftL:xs) = left (optimize xs) where
                      left (ShiftR:ys) = ys
                      left ys2 = ShiftL:ys2
optimize (ShiftR:xs) = right (optimize xs) where
                       right (ShiftL:ys) = ys
                       right ys2 = ShiftR:ys2

