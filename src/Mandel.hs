module Mandel where

import Data.Complex

mandelbrotFunction:: Complex Double -> Complex Double -> Complex Double
mandelbrotFunction z_n c = z_n * z_n + c

-- calculate the amount of complex number z
amountOfComplexNumber :: Complex Double -> Double
amountOfComplexNumber z = sqrt (a * a + b * b)
    where 
        a :: Double = realPart z
        b :: Double = imagPart z
        



iterateNTimes 0 z c = z
iterateNTimes n z c = iterateNTimes (n-1) (mandelbrotFunction z c) c
