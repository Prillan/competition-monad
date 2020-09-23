{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Data.Competition where

import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord
import Data.Void
import Data.Functor.Identity
import Control.Monad.Trans.Free
import Text.Read
import Data.List (sortBy)

type Count a = Map a Int

count :: Ord a => [a] -> Count a
count = M.fromListWith (+) . map (,1)

mostCommon :: Count a -> [(Int, a)]
mostCommon = sortBy (comparing (negate . fst))
  . map (\(a, b) -> (b, a))
  . M.toList

data MatchResult canTie = Home | Tie canTie | Away
  deriving (Show, Eq, Ord)

class Outcome canTie outcome where
  result :: outcome -> MatchResult canTie

winner :: id -> id -> MatchResult Void -> id
winner x y = \case
    Home -> x
    Away -> y

maybeWinner :: id -> id -> MatchResult x -> Maybe id
maybeWinner x y = \case
  Home  -> Just x
  Tie _ -> Nothing
  Away  -> Just y

data SimpleOutcome = SimpleOutcome { getResult :: MatchResult Void }
  deriving Show

instance Outcome Void SimpleOutcome where
  result = getResult

data SimpleOutcomeTie = SimpleOutcomeTie { getResultTie :: MatchResult () }
  deriving Show

instance Outcome () SimpleOutcomeTie where
  result = getResultTie

data ScoreOutcome = ScoreOutcome { scoreHome :: Int
                                 , scoreAway :: Int }
  deriving Show

instance Outcome () ScoreOutcome where
  result ScoreOutcome {..}
    | scoreHome > scoreAway  = Home
    | scoreHome == scoreAway = Tie ()
    | otherwise              = Away

data TournamentF id outcome next =
  Match id id (outcome -> next)
  deriving Functor

type CompT' m id outcome = FreeT (TournamentF id outcome) m
type Comp = CompT' Identity Int SimpleOutcome
type CompT m = CompT' m Int SimpleOutcome

match :: Monad m => id -> id -> CompT' m id outcome outcome
match x y = liftF (Match x y id)

matchWinner :: (Monad m, Outcome Void outcome) =>
  id -> id -> CompT' m id outcome id
matchWinner x y = winner x y . result <$> match x y

maybeMatchWinner :: (Monad m, Outcome () outcome) =>
  id -> id -> CompT' m id outcome (Maybe id)
maybeMatchWinner x y = maybeWinner x y . result @() <$> match x y

se4bracket :: (Monad m, Outcome Void outcome) =>
  a -> a -> a -> a -> CompT' m a outcome a
se4bracket p1 p2 p3 p4 = do
  winner1 <- matchWinner p1 p2
  winner2 <- matchWinner p3 p4

  matchWinner winner1 winner2

rrGroup :: (Monad m, Ord a, Outcome () outcome) =>
  [a] -> CompT' m a outcome [(Int, a)]
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
single _ = error "Too many players, wtf!?"

bracket :: Monad m => [id] -> CompT' m id SimpleOutcome id
bracket players = single <$> go players
  where step :: Monad m => [id] -> CompT' m id SimpleOutcome [id]
        step [] = pure []
        step [p] = pure [p]
        step (p1:p2:rest) =
          (:) <$> matchWinner p1 p2 <*> step rest

        go [] = pure []
        go [p] = pure [p]
        go xs = step xs >>= go


pureEvaluator :: (t -> t -> MatchResult Void)
              -> TournamentF t SimpleOutcome next
              -> next
pureEvaluator compute (Match x y f) = f (SimpleOutcome (compute x y))

monadEvaluator :: Monad m =>
  (t -> t -> m (MatchResult Void))
  -> TournamentF t SimpleOutcome (m b)
  -> m b
monadEvaluator compute (Match x y f) = (SimpleOutcome <$> compute x y) >>= f

ordEvaluator :: Ord a => TournamentF a SimpleOutcomeTie next -> next
ordEvaluator (Match x y f) =
  f $ SimpleOutcomeTie $ case compare x y of
                           GT -> Home
                           EQ -> Tie ()
                           LT -> Away

consoleDecider :: Show t => t -> t -> IO (MatchResult canTie)
consoleDecider x y = do
  putStrLn $ "Match between " ++ show x ++ " and " ++ show y
  putStr $ "Winner [h/a]: "
  w <- getLine
  case w of
    ('h':_) -> pure Home
    ('a':_) -> pure Away
    _ -> putStrLn "Invalid option!" >> consoleDecider x y

consoleDeciderTie :: Show t => t -> t -> IO (MatchResult ())
consoleDeciderTie x y = do
  putStrLn $ "Match between " ++ show x ++ " and " ++ show y
  putStr $ "Winner [h/t/a]: "
  w <- getLine
  case w of
    ('h':_) -> pure Home
    ('t':_) -> pure (Tie ())
    ('a':_) -> pure Away
    _ -> putStrLn "Invalid option!" >> consoleDeciderTie x y

run :: (Int -> Int -> MatchResult Void) -> Comp a -> a
run c = runIdentity . iterT (pureEvaluator c)

runM :: Monad m =>
  (Int -> Int -> m (MatchResult Void)) -> CompT m a -> m a
runM c = iterT (monadEvaluator c)

{-
Simulation:
>> (runM (\_ _ -> choose 0.55 Home Away) (bracket [1, 2, 3, 4])) :: T Rational Int
fromFreqs [(1,121 % 400),(2,99 % 400),(3,99 % 400),(4,81 % 400)]

>> let x = (runM (\_ _ -> R.pick $ choose 0.75 Home Away) (bracket [1..32]))
>> R.print $ R.dist (replicate 100 x)
fromFreqs [(1,0.20000000000000004),(17,0.11999999999999998),(10,8.0e-2),(2,7.0e-2),(5,7.0e-2),(3,5.0e-2),(9,5.0e-2),(7,4.0e-2),(11,4.0e-2),(13,4.0e-2),(18,4.0e-2),(4,3.0e-2),(25,3.0e-2),(6,2.0e-2),(15,2.0e-2),(21,2.0e-2),(27,2.0e-2),(8,1.0e-2),(12,1.0e-2),(19,1.0e-2),(20,1.0e-2),(29,1.0e-2),(30,1.0e-2)]
-}
