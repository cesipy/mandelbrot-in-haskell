module Main where

import Mandel
import Codec.Picture



main :: IO ()
main = do
   let useColors::Bool = True
   writePng "result.png" $ generateImage renderFunction width height