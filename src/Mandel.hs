module Mandel where

import Data.Complex

mandelbrotFunction:: Complex Double -> Complex Double -> Complex Double
mandelbrotFunction z_n c = z_n * z_n + c


-- calculate the amount of complex number z
magnitudeComplexNumber :: Complex Double -> Double
magnitudeComplexNumber z = sqrt (a * a + b * b)
    where 
        a :: Double = realPart z
        b :: Double = imagPart z


mandelbrotSetMembershipCheck :: Int -> Complex Double -> Complex Double -> Maybe(Complex Double)
mandelbrotSetMembershipCheck 0 z c = Just z
mandelbrotSetMembershipCheck n z c 
    | magnitudeComplexNumber z >= 2 = Nothing
    | otherwise = mandelbrotSetMembershipCheck (n-1) (mandelbrotFunction z c) c


generateComplexPlaneGrid ::Double -> Int -> [Complex Double]
--generateList n step = [0, step..n]
generateComplexPlaneGrid step n = 
    [x :+ y | x <- [0, step..(fromIntegral n)], y <- [0, step..(fromIntegral n)]]


