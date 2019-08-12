{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
module Chess.Event
    ( initialEvents
    , finishingEvents
    , waitingEvents
    , stunnedEvents
    , castingEvents
    , attackingEvents
    , movingEvents
    , dyingEvents
    , targettingEvents
    , isFinished
    , Event(..)
    , InitialState(..)
    )
where

import           Data.List
import           Data.Maybe
import qualified Data.Map                      as Map
import           Control.Arrow
import           Chess.Component
import           Data.Board                     ( findPath
                                                , findTarget
                                                , movePiece
                                                , distance
                                                , manhattan
                                                , Board
                                                )
data Event where
    Place ::Belongs -> (Int, Int) -> Piece -> Event
    Move ::(Int, Int) -> (Int, Int) -> Event
    Wait ::(Int, Int) -> Event
    ChangeTarget ::(Int, Int) -> (Int, Int) -> Event
    Attack ::(Int, Int) -> (Int, Int) -> Int -> Event
    Status ::(Int, Int) -> Status -> Event
    ChangeFocus ::(Int, Int) -> Focus (Int, Int) -> Event
    ChangeStatus ::(Int, Int) -> Maybe Status -> Event
    Die ::(Int, Int) -> Event
    Finish ::Winner -> Event
    Board ::Int -> Event
    Start ::Event
    deriving Show

data InitialState = InitialState {
    mys :: [((Int, Int), Piece)],
    ops :: [((Int, Int), Piece)],
    boardSize :: Int
}

initialEvents :: InitialState -> [Event]
initialEvents state = concat
    [ [Start]
    , [Board (boardSize state)]
    , map (uncurry (Place My)) (mys state)
    , map (uncurry (Place Op)) (ops state)
    ]

finishingEvents :: [Belongs] -> Maybe Event
finishingEvents bs = if (> 1) . length . group . sort $ bs
    then Nothing
    else pure . Finish . maybe Draw Winner $ listToMaybe bs

waitingEvents :: [Event]
waitingEvents = []


targettingEvents
    :: Board -> ((Int, Int), Priority, (Int, Int) -> Bool) -> [Event]
targettingEvents board (this, pri, isEnemy) = fromMaybe [] $ do
    target <- findTarget board (manhattan, this, isEnemy)
    pure [ChangeTarget this target]

stunnedEvents :: ((Int, Int), Int) -> [Event]
stunnedEvents (pos, n) =
    let n' = n - 1
    in  if n' < 1 then [ChangeStatus pos Nothing] else [Status pos (Stunned n')]


castingEvents = undefined

movingEvents :: Piece -> ((Int, Int), (Int, Int)) -> Board -> (Board, [Event])
movingEvents (attackRange -> r) (this, that) board = fromMaybe (board, []) $ do
    path <- findPath board (this, that)
    case path of
        (now : next : _ : _) ->
            pure (movePiece (now, next) board, [Move now next])
        _ -> Nothing

attackingEvents :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Board -> [Event]
attackingEvents (r, d) (this, that) board =
    fromMaybe [] $ pure [Attack this that d]

dyingEvents :: ((Int, Int), Int) -> [Event]
dyingEvents (pos, health) = [ Die pos | health < 1 ]

isFinished :: Event -> Bool
isFinished (Finish _) = True
isFinished _          = False
