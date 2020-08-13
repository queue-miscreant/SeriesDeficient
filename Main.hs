module Main where

import System.Environment
import Math.Sequence.Deficient

import Math.GenBase.Recur (Literal(..), linseq)
import Math.GenBase.Base (times, deficient)

main = do (list:col:p:_) <- getArgs
          let fn     = cutSeq' $ dropWhile (/='[') list
          let s      | head list == 'r' = linseq $ Literal $ read $ tail list
                     | otherwise        = read list :: [Integer]
          let (p',f) | take 2 p == "--" = (read $ tail p, times s) 
                     | otherwise        = (read p, deficient s)
          let modn   | p' > 1    = " mod " ++ p
                     | otherwise = ""
          case p' < 0 of
            True -> gifTable (fn ++ ".gif") (-p') f (read col)
            _    -> writeTable (fn ++ modn ++ ".png") p' s (read col)
