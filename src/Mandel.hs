module Mandel where

import Data.Complex
import Codec.Picture


mandelbrotFunction:: Complex Double -> Complex Double -> Complex Double
mandelbrotFunction z_n c = z_n * z_n + c


-- calculate the amount of complex number z
magnitudeComplexNumber :: Complex Double -> Double
magnitudeComplexNumber z = sqrt (a * a + b * b)
    where 
        a :: Double = realPart z
        b :: Double = imagPart z


generateComplexPlaneGrid ::Double -> Int -> [Complex Double]
--generateList n step = [0, step..n]
generateComplexPlaneGrid step n = 
    [x :+ y | x <- [0, step..(fromIntegral n)], y <- [0, step..(fromIntegral n)]]


iterateOverComplexGrid :: [Complex Double] -> [Complex Double]
iterateOverComplexGrid []     = []
iterateOverComplexGrid (x:xs) 
                    | mandelbrotSetMembershipCheck iterations z_0 x == True = x : iterateOverComplexGrid xs
                    | otherwise                                             =  iterateOverComplexGrid xs
    where
        iterations            = 20
        z_0:: Complex Double  = 0.0 :+ 0.0


{- function is used to convert the coordinates to a list of tuples [(Double, Double)]
-}
extractCoordinates:: [Complex Double] -> [(Double, Double)]
extractCoordinates [] = []
extractCoordinates (x:xs) = (realPart x, imagPart x) : extractCoordinates xs



{-------------------------}


mandelbrotSetMembershipCheck :: Int -> Complex Double -> Complex Double -> Bool
mandelbrotSetMembershipCheck 0 z c  = True
mandelbrotSetMembershipCheck n z c 
    | magnitudeComplexNumber z >= 2 = False
    | otherwise                     = mandelbrotSetMembershipCheck (n-1) (mandelbrotFunction z c) c



generatePixel x y = if mandelbrotSetMembershipCheck iterations 0 (createComplexNumber x y) == True 
        then PixelRGB8 100 100 100
        else PixelRGB8 0 0 0 
    where 
        iterations = 20

createComplexNumber :: Double ->  Double -> Complex Double
createComplexNumber x y = x :+ y


x_min = -2.0
x_max = 2
y_min = -2
y_max = 2

width :: Int
width = 4000

height :: Int
height = 4000

mapToBoundries:: Int -> Int -> (Double, Double)
mapToBoundries x y  =
            let newX  = x_min + fromIntegral x * ((x_max - x_min) / fromIntegral width)
                newY  = y_max - fromIntegral y * ((y_max - y_min) / fromIntegral height)
            in (newX, newY)


renderFunction :: Int -> Int -> PixelRGB8
renderFunction x y =  generatePixel (fst mappedPosition) (snd mappedPosition)
    where
        mappedPosition = mapToBoundries x y



