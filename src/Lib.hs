module Lib where

import qualified Data.Map                      as Map
import           Data.List
import           Control.Monad
import           Data.Ecstasy
import           World                          ( world )
import           Event                          ( InitialState(..) )
import           Chess.Component                ( Piece(..)
                                                , Level(..)
                                                )
import           System.Random
import           Control.Monad.State

debug = True

upper :: [((Int, Int), Piece)]
upper =
        [ ((2, 3), Farmer One)
        , ((2, 4), Farmer One)
        , ((2, 5), Farmer One)
        , ((2, 9), Peter Two)
        ]

lower :: [((Int, Int), Piece)]
lower = [((9, 3), Farmer One), ((9, 4), Farmer Two), ((9, 5), Farmer Three)]

run :: Bool -> Int -> IO ()
run debug seed = do
        let     state = InitialState { boardSize = 10
                                     , myPieces  = lower
                                     , opPieces  = upper
                                     }
                gen = mkStdGen seed
        events <- runStateT (runSystemT defStorage (world debug 100 state)) gen
        mapM_ print events
