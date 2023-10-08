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


mandelbrotSetMembershipCheck :: Int -> Complex Double -> Complex Double -> Bool
mandelbrotSetMembershipCheck 0 z c  = True
mandelbrotSetMembershipCheck n z c 
    | magnitudeComplexNumber z >= 2 = False
    | otherwise                     = mandelbrotSetMembershipCheck (n-1) (mandelbrotFunction z c) c


generateComplexPlaneGrid ::Double -> Int -> [Complex Double]
--generateList n step = [0, step..n]
generateComplexPlaneGrid step n = 
    [x :+ y | x <- [0, step..(fromIntegral n)], y <- [0, step..(fromIntegral n)]]


iterateOverComplexGrid :: [Complex Double] -> [Bool]
iterateOverComplexGrid []     = []
iterateOverComplexGrid (x:xs) = mandelbrotSetMembershipCheck iterations z_0 x : iterateOverComplexGrid xs
    where
        iterations            = 10
        z_0:: Complex Double  = 0.0 :+ 0.0