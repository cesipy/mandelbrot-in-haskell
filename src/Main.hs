module Main where

import Mandel
import Graphics.Gnuplot.Simple
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Codec.Picture



main :: IO ()
main = do
   -- BL.writeFile "results.csv" csvData 
    writePng "result.png" $ generateImage renderFunction width height
