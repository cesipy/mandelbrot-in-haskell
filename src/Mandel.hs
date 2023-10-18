module Mandel where

import Data.Complex
import Codec.Picture ( PixelRGB8(..) )

{------------------------------------------------}

x_min =  -0.75
x_max = -0.745
y_min = 0.1
y_max = 0.105

width  :: Int
width  = 2000

height :: Int
height = 2000

{------------------------------------------------}


-- | Calculates the next iteration of `z_n+1` with a given `z_n` and  `c`
mandelbrotFunction:: Complex Double -> Complex Double -> Complex Double
mandelbrotFunction z_n c = z_n * z_n + c


-- | Calculates the magnitude of complex number z
magnitudeComplexNumber :: Complex Double -> Double
magnitudeComplexNumber z = sqrt (a * a + b * b)
    where
        a :: Double = realPart z
        b :: Double = imagPart z


-- | Checks whether a complex number is a member of the Mandelbrot Set.
--
-- Given a starting point, a complex number, and an iteration counter this function
-- checks if the complex number remains within the Mandelbrot Set after a series
-- of iterations. 
mandelbrotSetMembershipCheck :: Int           -- ^ Number of iterations
                            -> Complex Double -- ^ Current complex number
                            -> Complex Double -- ^ Initial complex number
                            -> Bool
mandelbrotSetMembershipCheck 0 _ _  = True
mandelbrotSetMembershipCheck n z c
    | magnitudeComplexNumber z >= 2 = False
    | otherwise                     = mandelbrotSetMembershipCheck (n-1) (mandelbrotFunction z c) c


-- | Generates a complex number.
createComplexNumber :: Double ->  Double -> Complex Double
createComplexNumber x y = x :+ y


-- | Adjusts a number `i` to a specified range. (`maxBoundries`)
adjustSpan :: Int -> Int -> Int
adjustSpan i maxBoundries = i *  255 `div` maxBoundries


-- | Calculates the color for a complex number based on its position in the mandelbrot set.
calculateColor :: Int ->  Complex Double ->Complex Double -> Int -> Int
calculateColor iterations z_n c i
    | i > iterations                 = 0
    | magnitudeComplexNumber z_n >= 2.0 = 255 -  adjustSpan i iterations
    | otherwise                      = calculateColor iterations (mandelbrotFunction z_n c) c (i+1)


-- | Generates a Pixel with type `PixelRGB8` with RGB value `(x, x, x)`
generateRGB :: Int -> PixelRGB8
generateRGB x = PixelRGB8 (fromIntegral x) (fromIntegral x) (fromIntegral x)


-- | Generates a Pixel with type `PixelRGB8` with RGB value `(x, y, z)`
generateRGBdifferentColors :: Int -> PixelRGB8
generateRGBdifferentColors x = PixelRGB8 (fromIntegral x) (fromIntegral x `div` 3) (fromIntegral x `div` 2)


-- | Generates pixel for spedific x and y coordinates
generatePixel :: Double -> Double -> PixelRGB8
generatePixel x y = generateRGB (calculateColor maxIterations z_0 ( createComplexNumber x y) 0)
    where
        maxIterations = 170
        z_0           = createComplexNumber 0 0


-- | Maps `x`and `y` to a given `width` and `height`
mapToBoundries:: Int -> Int -> Int -> Int -> (Double, Double)
mapToBoundries x y width height  =
            let newX  = x_min + fromIntegral x * ((x_max - x_min) / fromIntegral width)
                newY  = y_max - fromIntegral y * ((y_max - y_min) / fromIntegral height)
            in (newX, newY)


renderFunction :: Int -> Int -> PixelRGB8
renderFunction x y =  generatePixel (fst mappedPosition) (snd mappedPosition)
    where
        mappedPosition = mapToBoundries x y width height