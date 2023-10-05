module Mandel where

import Data.Complex

mandelbrot_function:: Int -> Int -> Int
mandelbrot_function z_n c = (z_n * z_n + c)

