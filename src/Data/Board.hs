{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Board
    ( emptyBoard
    , findPath
    , findTarget
    , neighbours
    , placePieces
    , movePiece
    , distance
    , manhattan
    , invert
    , Board
    , Node(..)
    )
where

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Maybe
import           Data.List
import           Data.Function
import           Data.Ecstasy
import           Control.Arrow
import           Data.Tuple

data Node a = Node {
    value :: Maybe a,
    connected :: [(Int, Int)]
} deriving (Show, Eq)

type Board = Map.Map (Int, Int) (Node Ent)

neighbours :: Int -> (Int, Int) -> [(Int, Int)]
neighbours size (i, j) = filter
    inside
    [ (i + 1, j)
    , (i - 1, j)
    , (i    , j + 1)
    , (i    , j - 1)
    , (i + 1, j + 1)
    , (i - 1, j - 1)
    , (i + 1, j - 1)
    , (i - 1, j + 1)
    ]
    where inside (i', j') = i' >= 1 && i' <= size && j' >= 1 && j' <= size

emptyBoard :: Int -> Board
emptyBoard n = Map.fromList $ zipWith (curry createNode) ys xs
  where
    size       = take $ n * n
    xs         = size . cycle $ [1 .. n]
    ys         = size . concat . iterate ((+ 1) <$>) $ replicate n 1
    createNode = Node Nothing . neighbours n >>= flip (,)

placePieces :: Board -> [((Int, Int), Ent)] -> Board
placePieces = foldl'
    (\board (coord, e) -> Map.adjust (\n -> n { value = Just e }) coord board)

placePiece :: (Int, Int) -> Ent -> Board -> Board
placePiece pos e = Map.adjust
    (\n@Node { value } -> n
        { value = if isJust value
                      then error
                          "Trying to place piece, but position is occupied"
                      else Just e
        }
    )
    pos

removePiece :: (Int, Int) -> Board -> (Ent, Board)
removePiece = (.) ((,) <$> lookup <*> remove) . (,)
  where
    lookup =
        fromMaybe (error "Trying to remove piece, but position is vacant")
            . value
            . uncurry (flip (Map.!))
    remove = uncurry (Map.adjust (\n -> n { value = Nothing }))


movePiece :: ((Int, Int), (Int, Int)) -> Board -> Board
movePiece (from, to) = uncurry (placePiece to) . removePiece from


findTarget
    :: Board
    -> ((Int, Int) -> (Int, Int) -> Int, (Int, Int), (Int, Int) -> Bool)
    -> Maybe (Int, Int)
findTarget board (priority, pos, isTarget) =
    listToMaybe
        . filter isTarget
        . sortBy (compare `on` priority pos)
        . Map.keys
        $ board


invert :: Board -> Map.Map Ent (Int, Int)
invert = Map.fromList . mapMaybe (fmap swap . traverse value) . Map.toList

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

distance :: Board -> ((Int, Int), (Int, Int)) -> Int
distance board (start, end) = maybe maxBound (subtract 1 . length)
    $ astar start (== end) (const 1) (connected . (board Map.!))




findPath :: Board -> ((Int, Int), (Int, Int)) -> Maybe [(Int, Int)]
findPath board (start, end) = astar start
                                    (== end)
                                    (manhattan end)
                                    (connected . (board' Map.!))
  where
    board' = fmap
        (\n@Node { connected } ->
            n { connected = filter (`notElem` occupied) connected }
        )
        board
    occupied =
        map fst
            . filter ((&&) <$> isJust . value . snd <*> (/= end) . fst)
            $ Map.toList board


astar :: Ord a => a -> (a -> Bool) -> (a -> Int) -> (a -> [a]) -> Maybe [a]
astar start isEnd gValue nextNodes = go (Set.singleton (0, start))
                                        mempty
                                        mempty
  where
    go open closed tracks | Set.null open          = Nothing
                          | isEnd node             = Just $ trace tracks node
                          | Set.member node closed = go open' closed tracks
                          | otherwise              = go open'' closed' tracks'
      where
        ((g, node), open') = Set.deleteFindMin open
        closed'            = Set.insert node closed
        g'                 = (g +) . gValue
        next =
            filter
                    (\x -> Set.notMember x closed
                        && maybe True ((g' x <) . fst) (Map.lookup x tracks)
                    )
                $ nextNodes node
        open''  = foldl' (\m a -> Set.insert (g' a, a) m) open' next
        tracks' = foldl' (\m a -> Map.insert a (g' a, node) m) tracks next

trace :: Ord a => Map.Map a (b, a) -> a -> [a]
trace tracks node = fix
    (\rec nodes@(node : _) -> if Map.notMember node tracks
        then nodes
        else rec ((snd . (tracks Map.!) $ node) : nodes)
    )
    [node]
