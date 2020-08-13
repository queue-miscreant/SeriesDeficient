module Main where

import System.Environment
import Math.Sequence.Deficient

import Math.GenBase.Recur (Literal(..), linseq)
import Math.GenBase.Base (times, deficient)

cutSeq' = map (\n -> if n == ',' then ' ' else n) . init . tail
cutSeq :: Show a => [a] -> String
cutSeq = cutSeq' . show . take 10

main = do (list:col:p:_) <- getArgs
          let fn   = cutSeq' $ dropWhile (/='[') list
          let s    | head list == 'r' = linseq $ Literal $ read $ tail list
                   | otherwise        = read list :: [Integer]
          let f    | take 2 p == "--" = gifTable (times s)     (read $ drop 2 p) (fn ++ ".gif") 
                   | head p   == '-'  = gifTable (deficient s) (read $ tail p) (fn ++ ".gif") 
                   | otherwise        = pngTable (deficient s) (read p) (fn ++ modn ++ ".png") 
          let modn | p' > 1    = " mod " ++ p
                   | otherwise = ""
          f $ read col
