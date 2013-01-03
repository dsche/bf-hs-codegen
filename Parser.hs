{-# LANGUAGE NoImplicitPrelude #-}

module BFParser where

import BF
import Prelude (Char, Show, Eq, String, Maybe(..), map, (.), ($), tail)

data Cmd = P | M | L | R | I | O | B | E | Void
         deriving (Eq, Show)
--data Command = Plus | Minus | Left | Right | In | Out | Loop [Command]
--             deriving Show

type Input = [Cmd]

lex :: Char -> Cmd
lex '+' = P
lex '-' = M
lex '<' = L
lex '>' = R
lex '.' = O
lex ',' = I
lex '[' = B
lex ']' = E
lex _ = Void

parse :: Input -> ([Command], Input)
parse (P:xs) = let (p, r) = parse xs in (Plus:p, r)
parse (M:xs) = let (p, r) = parse xs in (Minus:p, r)
parse (L:xs) = let (p, r) = parse xs in (Left:p, r)
parse (R:xs) = let (p, r) = parse xs in (Right:p, r)
parse (I:xs) = let (p, r) = parse xs in (In:p, r)
parse (O:xs) = let (p, r) = parse xs in (Out:p, r)
parse (Void:xs) = parse xs
parse (E:xs) = ([], E:xs) -- semi-error
parse (B:xs) = case rest of
  (E:rest') -> ((Loop subtree):p, r)
  otherwise -> ([], B:xs)
  where (p, r) = parse (tail rest)
        (subtree, rest) = parse xs
parse [] = ([], [])

parseProgram :: String -> Maybe Program
parseProgram str = case (parse . map lex) str of
  (cmds, []) -> Just $ cmds
  (_, x:_) -> Nothing
