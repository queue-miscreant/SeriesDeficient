module Math.Sequence.Deficient where

import Prelude
import Codec.Picture
import Data.List (elemIndex)
import Data.Ratio
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV

import Math.GenBase.Base (times, deficient)

--prime factorizations for integers including 1 and 0
--This is a very bad idea, but oh well
factorized :: [[Int]]
factorized = [0]:[1]:factorized' [] [2..]
  where factorized' ps (n:ns) 
         | null factor  = [n]:factorized' (n:ps) ns
         | otherwise    = factor:factorized' ps ns
           where factor = factor' n $ reverse ps
                 factor' n []        = []
                 factor' 1 _         = []
                 factor' n ps@(p:pp) | rem == 0  = p:factor' div ps
                                     | otherwise = factor' n pp
                    where (div,rem) = quotRem n p
primes = map head $ filter ((<=1) . length) factorized

--convert a number to an rgb based on its prime factorization
--XXX you know, hue only works when the range you're using is cyclic; it'd work
--for galois fields or complex arguments, but I don't know a good color space
primeColor x | x > 50000 = hsv 0 0 0
             | otherwise = case elemIndex hue primes of {
    Just hue' -> hsv (360*(1-(1-1/360)^^hue')) sat 1;
    --Just hue' -> hsv (fromIntegral hue') sat 1;
    Nothing   -> hsv 0 0 0
  } where factorized' = factorized !! abs x
          --the fewer factors a number has, the more "saturated" it is
          sat  = recip $ fromIntegral $ length factorized'
          hue  = last factorized' --factorized is already ordered
          --(fromIntegral hue) * 360 / (fromIntegral largestAngle)
          --largestAngle = primes !! 360

rgbF8 (RGB r g b)  = PixelRGB8 r' g' b'
  where [r',g',b'] = map (round . (255*)) [r,g,b]

writeTable fn p s col = writePng fn $ generateImage (((rgbF8 . f) .) . deficient s) col col
  where f | p <= 1    = primeColor
          | otherwise = \x -> hsv (fromIntegral (x `mod` p) * 360 / fromIntegral p) 1 1

{-
defaultWrite p s col = writeTable (cutSeq s ++ modn ++ ".png") p s col
  where modn | p > 1     = " mod " ++ show p
             | otherwise = ""
-}

spectrumize p = round . (* (255 % p)) . fromIntegral . flip mod p

gifTable fn fr ff col = either putStrLn id $ writeGifImages fn LoopingNever frames
  where palette = generateImage (\n _ -> rgbF8 $ hsv (fromIntegral n*360/255) 1 1) 256 1
        cache   = [[ff a b | a <- [0..col-1]] | b <- [0..col-1]]
        frame p = (spectrumize p .) . (!!) . (!!) cache
        frames  = [(palette, 15, generateImage (frame n) col col) | n <- [2..fr+1]]

{- No surprise, but the bottleneck is writeGifImages, not caching frames
 -
gifTableResponsive fn fr ff col = do {
     putStrLn $ "Caching table within dimensions (" ++ show col ++ ")";
     --force evaluation
     cache' <- return $ if seq cache True then cache else error "Cache failed!";
     putStrLn "Generating frames";
     frames' <- return $ if seq frames True then frames else error "Frames failed!";
     putStrLn "Writing GIF";
     either putStrLn id $ writeGifImages fn LoopingNever frames';
  } where palette   = generateImage (\n _ -> rgbF8 $ hsv (fromIntegral n*360/255) 1 1) 256 1;
          frame c p = generateImage ((spectrumize p .) . ((!!) . (!!) c)) col col
          frames   = [(palette, 15, frame cache n) | n <- [2..fr+1]]
          cache    = [[ff a b | a <- [0..col-1]] | b <- [0..col-1]]
-}

canonical = \fn fr -> gifTable fn fr (*)

cutSeq' = map (\n -> if n == ',' then ' ' else n) . init . tail
cutSeq :: Show a => [a] -> String
cutSeq = cutSeq' . show . take 10
