module Main where

import           Lib
import           Data.List
import           System.Random

main :: IO ()
main = do
    g <- newStdGen
    run False (fst . next $ g)
