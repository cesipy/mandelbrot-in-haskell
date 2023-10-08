module Main where

import Mandel
import Graphics.Gnuplot.Simple
import qualified Data.ByteString.Lazy as BL
import Data.Csv


grid                  = generateComplexPlaneGrid 0.001 2
iterated_grid         = iterateOverComplexGrid grid
extracted_coordinates = extractCoordinates iterated_grid

csvData = encode extracted_coordinates



main :: IO ()
main = 
    BL.writeFile "results.csv" csvData