{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
module Helper.Any where

import           Control.Monad.Writer
import           Control.Monad.Reader
import           Data.Maybe
import           Data.List


takeWhileM :: Monad m => (a -> Bool) -> [m a] -> m [a]
takeWhileM _      []         = pure []
takeWhileM isDone (ma : mas) = do
    a <- ma
    if isDone a then pure [a] else (a :) <$> takeWhileM isDone mas

instance Functor ((,,) b c) where
    fmap f (b, c, a) = (b, c, f a)

deriving instance Foldable ((,,) b c)

deriving instance Traversable ((,,) b c)


toMaybe bool a = if bool then Just a else Nothing
