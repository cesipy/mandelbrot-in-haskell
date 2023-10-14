module Main where

import Mandel
import qualified Data.ByteString.Lazy as BL
import Codec.Picture



main :: IO ()
main = do
   -- BL.writeFile "results.csv" csvData 
   writePng "result.png" $ generateImage renderFunction width height