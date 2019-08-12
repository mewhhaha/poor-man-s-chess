{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Chess.Component where

import           Data.Ecstasy
import qualified Data.Map                      as Map
import Data.Board

class Stats a where
    name :: a -> String
    cost :: a -> Maybe Int
    attackDamage :: a -> (Int, Int)
    attackRange :: a -> Int
    maxHealth :: a -> Int
    maxMana :: a -> Int
    member :: a -> [Member]
    priority :: a -> Priority
    level :: a -> Level

class Family a where
    familyBonus ::  a -> Int -> Level

data Level = One | Two | Three | Four | Five
    deriving (Show, Enum)

data Winner = Winner Belongs | Draw
    deriving Show

data Member where
    Hay ::Member
    Human ::Member
    Cattle ::Member
    Traitor ::Member
    Drunk ::Member
    Cursed ::Member
    deriving (Show, Eq, Ord)

data Piece where
    Farmer ::Level -> Piece
    Peter ::Level -> Piece
    Cow ::Level -> Piece
    Haybale ::Level -> Piece
    Nercow ::Piece
  deriving Show

data Belongs = My | Op
    deriving (Show, Ord, Eq, Enum)

data Priority = Closest | Furthest | None | Random

data Status = Stunned Int | Silenced Int
    deriving Show

data Focus a = Attacking a | Approaching a | Casting (Int, Int) | Waiting
    deriving Show



type Field s a = Component s 'Field a
type Unique s a = Component s 'Unique a

data World s = Entity {
    position :: Field s (Int, Int),
    belongs :: Field s Belongs,
    health :: Field s Int,
    target :: Field s (Maybe Ent),
    status :: Field s (Maybe Status),
    mana :: Field s Int,
    piece :: Field s Piece,
    board :: Unique s Board
    } deriving (Generic)

type Entity = World 'FieldOf

instance Stats Piece where
    name = \case
        (Farmer  _) -> "Farmer"
        (Peter   _) -> "Peter"
        (Cow     _) -> "Cow"
        (Haybale _) -> "Haybale?"
        Nercow      -> "Nercow"

    attackDamage = \case
        (Farmer  lvl) -> pick lvl [(5, 15), (10, 20), (20, 30)]
        (Peter   lvl) -> pick lvl [(10, 10), (5, 30), (2, 50)]
        (Cow     lvl) -> pick lvl [(0, 0), (0, 0), (0, 0)]
        (Haybale lvl) -> pick lvl [(5, 20), (10, 30), (20, 40)]
        Nercow        -> (25, 35)

    maxHealth = \case
        (Farmer  lvl) -> pick lvl [100, 160, 300]
        (Peter   lvl) -> pick lvl [65, 170, 200]
        (Cow     lvl) -> pick lvl [1, 1, 1]
        (Haybale lvl) -> pick lvl [70, 100, 120]
        Nercow        -> 200

    maxMana = \case
        (Farmer  _) -> 100
        (Peter   _) -> 100
        (Cow     _) -> 100
        (Haybale _) -> 100
        Nercow      -> 0

    attackRange = \case
        (Farmer  _) -> 1
        (Peter   _) -> 3
        (Cow     _) -> 0
        (Haybale _) -> 1
        Nercow      -> 1

    member = \case
        (Farmer  _) -> [Human, Hay]
        (Peter   _) -> [Human, Drunk]
        (Cow     _) -> [Cattle, Hay]
        (Haybale _) -> [Traitor, Drunk]
        Nercow      -> [Cattle, Cursed]

    priority = \case
        (Farmer  _) -> Closest
        (Peter   _) -> Random
        (Cow     _) -> None
        (Haybale _) -> Furthest
        Nercow      -> Closest

    level = \case
        (Farmer  lvl) -> lvl
        (Peter   lvl) -> lvl
        (Cow     lvl) -> lvl
        (Haybale lvl) -> lvl
        Nercow        -> Three

    cost = \case
        (Farmer  _) -> pure 1
        (Peter   _) -> pure 2
        (Cow     _) -> pure 2
        (Haybale _) -> pure 1
        Nercow      -> Nothing

instance Family Member where
    familyBonus = \case
        Hay     -> toEnum . max 3 . every 2
        Human   -> toEnum . max 3 . every 2
        Cursed  -> toEnum . only 1
        Cattle  -> toEnum . only 1
        Drunk   -> toEnum . max 3 . every 2
        Traitor -> toEnum . max 3 . every 2

every :: Int -> Int -> Int
every y x = x `div` y

more :: Int -> Int -> Int
more x y = fromEnum (y > x)

pick :: Enum a => a -> [b] -> b
pick = flip (!!) . fromEnum

only :: Int -> Int -> Int
only n = fromEnum . (== n)
