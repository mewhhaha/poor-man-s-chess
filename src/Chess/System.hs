{-# LANGUAGE LambdaCase #-}
module Chess.System where

import           Data.Ecstasy
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.List
import           Control.Monad
import           Control.Arrow
import           Data.Either
import           Helper.Ecstasy
import           Helper.Any
import           Chess.Component
import           Chess.Entity
import           Chess.Event
import           Data.Board
import           System.Random
import           Control.Monad.State
import           Debug.Trace


type System' = SystemT World (StateT StdGen IO)
type EventSystem = SystemT World (StateT StdGen IO) [Event]
type Query' = QueryT World (StateT StdGen IO)

splitBySide :: b -> Query' (Either b b)
splitBySide a = ([Left a, Right a] !!) . fromEnum <$> query belongs

withBoard :: (Board -> System' a) -> System' a
withBoard f = do
    board           <- fromJust . listToMaybe <$> efor allEnts (query board)
    boardWithPieces <- placePieces board
        <$> efor allEnts (with piece >> ((,) <$> query position <*> queryEnt))
    f boardWithPieces

combineSystems :: Monad m => [m [a]] -> m [a]
combineSystems = (concat <$>) . sequence

initSystem :: InitialState -> EventSystem
initSystem = pure . initialEvents

finishSystem :: EventSystem
finishSystem =
    maybeToList . finishingEvents <$> efor allEnts (with piece >> query belongs)

movementSystem :: Board -> EventSystem
movementSystem board =
    fmap (concat . snd) . flip (efold allEnts) (board, []) $ \(b, events) -> do
        (pie, this, tar) <- query3 piece position target

        let merge (b', es) = pure (b', es : events)

        merge $ fromMaybe (b, []) $ do
            that <- flip Map.lookup (invert b) =<< tar
            toMaybe (distance board (this, that) > attackRange pie)
                    (movingEvents pie (this, that) b)


targetSystem :: Board -> EventSystem
targetSystem board = do
    (myPieces, opPieces) <- partitionEithers
        <$> efor allEnts (with piece >> query position >>= splitBySide)

    let isEnemy bel =
            let opposing = ([opPieces, myPieces] !!) . fromEnum
            in  (`Set.member` Set.fromList (opposing bel))

    fmap concat . efor allEnts $ do
        (pie, pos, tar, whose) <- query4 piece position target belongs
        pure $ targettingEvents board (pos, priority pie, isEnemy whose)

attackSystem :: Board -> EventSystem
attackSystem board = do
    splitGen <- splitRandomGen

    fmap (concat . snd) . flip (efold allEnts) (splitGen, []) $ \(g, events) ->
        do
            (pie, this, tar) <- query3 piece position target

            let merge (g', es) = pure (g', es : events)

            merge $ fromMaybe (g, []) $ do
                that <- flip Map.lookup (invert board) =<< tar
                let (i, g') = randomR (attackDamage pie) g
                toMaybe
                    (distance board (this, that) <= attackRange pie)
                    ( g'
                    , attackingEvents (attackRange pie, i) (this, that) board
                    )


deathSystem :: EventSystem
deathSystem = concat
    <$> efor allEnts (with piece >> dyingEvents <$> query2 position health)

statusSystem :: EventSystem
statusSystem = fmap concat $ efor allEnts $ do
    (_, pos, sts) <- query3 piece position status
    pure $ case sts of
        Just (Stunned n) -> stunnedEvents (pos, n)
        Nothing          -> []

evalSystem :: Board -> [Event] -> System' ()
evalSystem board = mapM_ evalEvent
  where
    evalEvent :: Event -> System' ()
    evalEvent = \case
        Start                  -> return ()
        (Board size          ) -> void $ createEntity $ newBoard size
        (Place whose coord pz) -> void $ createEntity (newPiece whose coord pz)
        (Finish _            ) -> return ()
        (Move this to        ) -> mapEnt this $ setPosition to
        (Attack _ that damage) -> mapEnt that $ modifyHealth (subtract damage)
        (ChangeTarget this target) ->
            let that = getEnt target in mapEnt this $ setTarget (Just that)
        (ChangeStatus this status) -> mapEnt this $ setStatus status
        Die this                   -> mapEnt this $ pure delEntity
      where
        hasEnt = isJust . value . (board Map.!)
        getEnt =
            fromMaybe (error "Couldn't get ent in evalEvent")
                . value
                . (board Map.!)
        mapEnt = emap . anEnt . getEnt
        setTarget a = pure $ unchanged { target = Set a }
        setPosition a = pure $ unchanged { position = Set a }
        setStatus a = pure $ unchanged { status = Set a }
        modifyHealth f = do
            h <- query health
            pure $ unchanged { health = Set (f h) }
