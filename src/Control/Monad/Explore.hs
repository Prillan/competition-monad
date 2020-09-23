{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Explore ( Explore
                             , choose
                             , remaining )where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Data.Set.Monad (Set)
import qualified Data.Set.Monad as S
import Data.Containers.ListUtils (nubOrd)

newtype Explore a = Explore { runExplore :: Set a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

choose :: Ord a => [a] -> Explore a
choose = Explore . S.fromList

remaining :: Ord a => Explore a -> [a]
remaining = S.toList . runExplore
