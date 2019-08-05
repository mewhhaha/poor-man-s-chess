module Chess.Entity
    ( newPiece
    , newBoard
    )
where

import           Chess.Component
import           Data.Ecstasy
import           Board                          ( emptyBoard )

newPiece :: Belongs -> (Int, Int) -> Piece -> Entity
newPiece whose pos pz = newEntity { position = Just pos
                                  , piece    = Just pz
                                  , status   = Just Nothing
                                  , health   = Just (maxHealth pz)
                                  , mana     = Just (maxMana pz)
                                  , belongs  = Just whose
                                  , target   = Just Nothing
                                  }

newBoard :: Int -> Entity
newBoard size = newEntity { board = Just (emptyBoard size) }
