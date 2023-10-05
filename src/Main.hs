module Main where

import Mandel
import Control.Monad.RWS (MonadState(put))


main::IO()
main = do 
    putStrLn "hello"