{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Data.Competition where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Reader (ReaderT(runReaderT), ask)
import Data.Functor.Identity
import Data.List (sortBy)
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord
import Data.Void
import Text.Read

type Count a = Map a Int

count :: Ord a => [a] -> Count a
count = M.fromListWith (+) . map (,1)

mostCommon :: Count a -> [(Int, a)]
mostCommon = sortBy (comparing (negate . fst))
  . map (\(a, b) -> (b, a))
  . M.toList

newtype Player = Player Int
  deriving (Show, Read, Eq, Ord)

data MatchResult canTie = Home | Tie canTie | Away
  deriving (Show, Eq, Ord)

winner :: id -> id -> MatchResult Void -> id
winner x y = \case
    Home -> x
    Away -> y

maybeWinner :: id -> id -> MatchResult x -> Maybe id
maybeWinner x y = \case
  Home  -> Just x
  Tie _ -> Nothing
  Away  -> Just y

newtype PureEvaluatorT m a
  = PureEvaluatorT (ReaderT (Player -> Player -> MatchResult Void) m a)
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

runPureEvaluator f (PureEvaluatorT comp) = runReaderT comp f

class Monad m => MonadSimpleMatch m where
  match :: Player -> Player -> m (MatchResult Void)

instance Monad m => MonadSimpleMatch (PureEvaluatorT m) where
  match x y = PureEvaluatorT $ do
    f <- ask
    pure $ f x y

matchWinner :: MonadSimpleMatch m => Player -> Player -> m Player
matchWinner x y = winner x y <$> match x y

class Monad m => MonadMatch m where
  match' :: Player -> Player -> m (MatchResult ())

instance Monad m => MonadMatch (PureEvaluatorT m) where
  match' x y = conv <$> match x y
    where conv = \case
            Home -> Home
            Away -> Away

maybeMatchWinner :: MonadMatch m => Player -> Player -> m (Maybe Player)
maybeMatchWinner x y = maybeWinner x y <$> match' x y

se4bracket
  :: MonadSimpleMatch m =>
     Player -> Player -> Player -> Player -> m Player
se4bracket p1 p2 p3 p4 = do
  winner1 <- matchWinner p1 p2
  winner2 <- matchWinner p3 p4

  matchWinner winner1 winner2

rrGroup :: MonadMatch m => [Player] -> m [(Int, Player)]
rrGroup ps = do
  results <- sequence $ map (uncurry maybeMatchWinner) (orderedPairs ps)
  let base = M.fromList $ map (,0) ps
  pure (mostCommon . M.unionWith (+) base . count . concatMap maybeToList $ results)

orderedPairs :: [x] -> [(x, x)]
orderedPairs [] = []
orderedPairs [x] = []
orderedPairs (x:rest) = map (x,) rest ++ orderedPairs rest

single [x] = x
single [] = error "No players!"
single _ = error "Too many players..."

bracket :: MonadSimpleMatch m => [Player] -> m Player
bracket players = single <$> go players
  where step [] = pure []
        step [p] = pure [p]
        step (p1:p2:rest) =
          (:) <$> matchWinner p1 p2 <*> step rest

        go [] = pure []
        go [p] = pure [p]
        go xs = step xs >>= go


newtype ConsoleEvaluatorT m a
  = ConsoleEvaluatorT { runConsoleEvaluatorT :: m a }
  deriving newtype (Functor, Applicative, Monad)
  deriving MonadTrans via IdentityT

instance MonadIO m => MonadSimpleMatch (ConsoleEvaluatorT m) where
  match x y = ConsoleEvaluatorT . liftIO $ consoleDecider x y
    where consoleDecider x y = do
            putStrLn $ "Match between " ++ show x ++ " and " ++ show y
            putStr $ "Winner [h/a]: "
            w <- getLine
            case w of
              ('h':_) -> pure Home
              ('a':_) -> pure Away
              _ -> putStrLn "Invalid option!" >> consoleDecider x y

instance MonadIO m => MonadMatch (ConsoleEvaluatorT m) where
  match' x y = ConsoleEvaluatorT . liftIO $ consoleDeciderTie x y
    where consoleDeciderTie x y = do
            putStrLn $ "Match between " ++ show x ++ " and " ++ show y
            putStr $ "Winner [h/t/a]: "
            w <- getLine
            case w of
              ('h':_) -> pure Home
              ('t':_) -> pure (Tie ())
              ('a':_) -> pure Away
              _ -> putStrLn "Invalid option!" >> consoleDeciderTie x y


newtype OrdEvaluatorT m a
  = OrdEvaluatorT { runOrdEvaluatorT :: m a }
  deriving newtype (Functor, Applicative, Monad)
  deriving MonadTrans via IdentityT

instance Monad m => MonadMatch (OrdEvaluatorT m) where
  match' x y = OrdEvaluatorT . pure $
    case compare x y of
      GT -> Home
      EQ -> Tie ()
      LT -> Away


-- run :: (Int -> Int -> MatchResult Void) -> Comp a -> a
-- run c = runIdentity . iterT (pureEvaluator c)

-- runM :: Monad m =>
--   (Int -> Int -> m (MatchResult Void)) -> CompT m a -> m a
-- runM c = iterT (monadEvaluator c)

{-
Simulation:
>> (runM (\_ _ -> choose 0.55 Home Away) (bracket [1, 2, 3, 4])) :: T Rational Int
fromFreqs [(1,121 % 400),(2,99 % 400),(3,99 % 400),(4,81 % 400)]

>> let x = (runM (\_ _ -> R.pick $ choose 0.75 Home Away) (bracket [1..32]))
>> R.print $ R.dist (replicate 100 x)
fromFreqs [(1,0.20000000000000004),(17,0.11999999999999998),(10,8.0e-2),(2,7.0e-2),(5,7.0e-2),(3,5.0e-2),(9,5.0e-2),(7,4.0e-2),(11,4.0e-2),(13,4.0e-2),(18,4.0e-2),(4,3.0e-2),(25,3.0e-2),(6,2mmmm.0e-2),(15,2.0e-2),(21,2.0e-2),(27,2.0e-2),(8,1.0e-2),(12,1.0e-2),(19,1.0e-2),(20,1.0e-2),(
////29,1.0e-2),(30,1.0e-2)]
-}
