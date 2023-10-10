module Main where

import Mandel
import Graphics.Gnuplot.Simple
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Codec.Picture



grid                  = generateComplexPlaneGrid 0.001 2
iterated_grid         = iterateOverComplexGrid grid
extracted_coordinates = extractCoordinates iterated_grid

csvData = encode extracted_coordinates


main :: IO ()
main = do
    let width  = 400
        height = 400
   -- BL.writeFile "results.csv" csvData 
    writePng "result.png" $ generateImage renderFunction width height
