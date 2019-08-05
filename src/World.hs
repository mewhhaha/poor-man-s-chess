{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module World where

import qualified Data.Map                      as Map
import           Data.List
import           Data.Function
import           Data.Maybe
import qualified Data.Set                      as Set
import           Data.Ecstasy
import           Data.Traversable               ( for )
import           Data.Foldable                  ( for_ )
import           Data.Tuple                     ( swap )
import           Data.Either
import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Arrow
import           Control.Concurrent
import           Chess.Entity
import           Chess.Component
import           Chess.System
import           Board
import           Helper.Any
import           Event

eval :: EventSystem -> ([Event] -> System' ()) -> EventSystem
eval sys ev = do
    events <- sys
    ev events
    pure events

world :: Bool -> Int -> InitialState -> System' [[Event]]
world debug maxSteps state = takeWhileM
    (any isFinished)
    (take
        maxSteps
        (start state : repeat (step >>= (>>) <$> when debug . draw <*> return))
    )

start :: InitialState -> System' [Event]
start state = eval (initSystem state) (evalSystem undefined)

step :: System' [Event]
step = withBoard $ \board -> eval
    (combineSystems
        [ targetSystem board
        , movementSystem board
        , attackSystem board
        , statusSystem
        , deathSystem
        , finishSystem
        ]
    )
    (evalSystem board)

draw :: [Event] -> System' ()
draw events = withBoard $ \board -> do
    drawnEnts <- mapM (mapM draw') $ flatten board
    liftIO $ putStr "\ESC[2J"
    liftIO $ mapM_ print events
    liftIO $ mapM_ (putStrLn . unwords) drawnEnts
    liftIO $ threadDelay 1000000
  where
    flatten =
        fmap (value . snd <$>)
            . groupBy ((==) `on` (fst . fst))
            . sortBy (compare `on` fst)
            . Map.toList
    draw' = maybe (pure "[Vacant]") ((fromJust <$>) . (`runQueryT` queryName))
    queryName = do
        p <- query piece
        b <- query belongs
        pure
            $  "["
            <> show b
            <> " "
            <> take 1 (name p)
            <> " "
            <> show (1 + fromEnum (level p))
            <> "]"




