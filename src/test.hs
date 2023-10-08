import Mandel
import Data.Complex
--import Graphics


mandelTest = mandelbrotFunction z_0 c
    where
        z_0:: Complex Double = 0.0 :+ 0
        c  :: Complex Double = 1.0 :+ 1.2


