{-# LANGUAGE MonoLocalBinds #-}
module Helper.Ecstasy where

import           Data.Ecstasy
import           Data.Ecstasy.Internal
import           Control.Monad
import           Data.Maybe
import           Control.Monad.State
import           System.Random

query2 a b = (,) <$> query a <*> query b

query3 a b c = (,,) <$> query a <*> query b <*> query c

query4 a b c d = (,,,) <$> query a <*> query b <*> query c <*> query d


efold
    :: (HasWorld world m, Monad m)
    => EntTarget world m
    -> (b -> QueryT world m b)
    -> b
    -> SystemT world m b
efold ents f b =
    efor ents queryEnt
        >>= foldM (\b ent -> fromMaybe b <$> runQueryT ent (f b)) b

genRandomR :: (Random a, Monad m) => (a, a) -> SystemT w (StateT StdGen m) a
genRandomR range = do
    g <- get
    let (i, g') = randomR range g
    modify $ const g'
    return i

splitRandomGen :: Monad m => SystemT w (StateT StdGen m) StdGen
splitRandomGen = do
    g <- get
    let (sg, g') = split g
    modify $ const g'
    return sg

