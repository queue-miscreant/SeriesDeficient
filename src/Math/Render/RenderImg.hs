module Math.Render.RenderImg (pngTable, gifTable, canonical) where

import Prelude
import Codec.Picture
import Data.List (elemIndex)
import Data.Ratio
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV

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
--hue corresponds to the largest number in the prime expansion
--saturation corresponds to the number of terms in the prime expansion
primeColor x | x > 50000 = hsv 0 0 0
             | otherwise = case elemIndex hue primes of {
    Just hue' -> hsv (360*(1-(1-1/360)^^hue')) sat 1;
    --Just hue' -> hsv (fromIntegral hue') sat 1;
    Nothing   -> hsv 0 0 0
  } where factorized' = factorized !! abs x
          --the fewer factors a number has, the more "saturated" it is
          sat  = recip $ fromIntegral $ length factorized'
          hue  = last factorized' --factorized is already ordered

--XXX you know, hue only works when the range you're using is cyclic; it'd work
--for galois fields or complex arguments, but I don't know of a good color space

--converts RGB to pixel RGB values
rgbF8 (RGB r g b)  = PixelRGB8 r' g' b'
  where [r',g',b'] = map (round . (255*)) [r,g,b]

--helper function for GIF spectrum
spectrumize p = round . (* (255 % p)) . fromIntegral . flip mod p

--writes a png corresponding to the output of `ff` at the image coordinates
--mod `p`. If `p` is zero, an alternate coloring system based on prime expansion
--will be used
pngTable ff p fn col  = writePng fn $ generateImage ((f .) . ff) col col
  where modP x        = hsv (fromIntegral (x `mod` p) * 360 / fromIntegral p) 1 1
        colorize
          | p <= 1    = rgbF8 . primeColor
          | otherwise = rgbF8 . modP

--writes a gif
gifTable ff fr fn col = either putStrLn id $ writeGifImages fn LoopingNever frames
  where palette = generateImage (\n _ -> rgbF8 $ hsv (fromIntegral n*360/255) 1 1) 256 1
        cache   = [[ff a b | a <- [0..col-1]] | b <- [0..col-1]]
        frame p = (spectrumize p .) . (!!) . (!!) cache
        frames  = [(palette, 15, generateImage (frame n) col col) | n <- [2..fr+1]]

--the "canonical" product table, i.e. one with no errors
canonical = gifTable (*)
