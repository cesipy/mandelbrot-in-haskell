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




mandelbrotSetMembershipCheck :: Int -> Complex Double -> Complex Double -> Bool
mandelbrotSetMembershipCheck 0 z c  = True
mandelbrotSetMembershipCheck n z c 
    | magnitudeComplexNumber z >= 2 = False
    | otherwise                     = mandelbrotSetMembershipCheck (n-1) (mandelbrotFunction z c) c



generatePixel :: Double -> Double -> PixelRGB8
generatePixel x y = case mandelbrotSetMembershipCheck iterations 0 (createComplexNumber x y) of
        True  -> generateRGB ( fromIntegral $ calculateColor (iterations*3) 0 (createComplexNumber x y) 1 )
        False -> generateRGB 0
    where 
        iterations = 100


createComplexNumber :: Double ->  Double -> Complex Double
createComplexNumber x y = x :+ y


calculateColor :: Int ->  Complex Double ->Complex Double -> Int -> Int
calculateColor iterations z_n c i
    | i > iterations                 = 255
    | magnitudeComplexNumber z_n > 2 = adjustSpan i iterations
    | otherwise                      = calculateColor iterations (mandelbrotFunction z_n c) c (i+1)


adjustSpan :: Int -> Int -> Int
adjustSpan i maxIterations = i * 1000 *  255 `div` maxIterations * 1000


generateRGB :: Int -> PixelRGB8
generateRGB x = PixelRGB8 (fromIntegral x) (fromIntegral x) (fromIntegral x)



x_min = -1.5
x_max = -1.0
y_min = 0.0
y_max = 0.5

width :: Int
width = 6000

height :: Int
height = 6000

mapToBoundries:: Int -> Int -> (Double, Double)
mapToBoundries x y  =
            let newX  = x_min + fromIntegral x * ((x_max - x_min) / fromIntegral width)
                newY  = y_max - fromIntegral y * ((y_max - y_min) / fromIntegral height)
            in (newX, newY)


renderFunction :: Int -> Int -> PixelRGB8
renderFunction x y =  generatePixel (fst mappedPosition) (snd mappedPosition)
    where
        mappedPosition = mapToBoundries x y



