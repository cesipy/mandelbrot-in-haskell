
import Mandel
import Data.Complex


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
