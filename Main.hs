module Main where

import System.Environment
import Deficient.Render

import Math.Base.Recur (Literal(..), linseq)
import Math.Base.Base (times, deficient)

cutSeq' = map (\n -> if n == ',' then ' ' else n) . init . tail
cutSeq :: Show a => [a] -> String
cutSeq = cutSeq' . show . take 10

main = do (list:col:p:_) <- getArgs
          let fn   = cutSeq' $ dropWhile (/='[') list
          let s    | head list == 'r' = linseq $ Literal $ read $ tail list
                   | otherwise        = read list :: [Integer]
          let f    | take 2 p == "--" = gifTable (times s)     (read $ drop 2 p) gifFn
                   | head p   == '-'  = gifTable (deficient s) (read $ tail p) gifFn
                   | otherwise        = pngTable (deficient s) (read p) pngFn
                  where pngFn = fn ++ " mod " ++ p ++ ".png"
                        gifFn = fn ++ ".gif"
          f $ read col
