{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Competition.Uefa where

import Data.List (sortBy, sortOn, groupBy)
import Data.Competition
import Data.Ord (comparing)

import Data.Bifunctor (second)
import Data.Functor.Identity
import Control.Monad ((>=>))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State (StateT, evalStateT, runStateT, modify, get, modify')


groupOn x = groupBy (\a b -> x a == x b)

type Evaluator = Player -> Player -> (Int, Int)

idEval (Player p1) (Player p2) = (p1, 0)

class Monad m => MonadScoreMatch m where
  scoreMatch :: Player -> Player -> m ScoreMatch

data ScoreMatch  = ScoreMatch { mPlayerA :: Player
                              , mPlayerB :: Player
                              , mScoreA :: Int
                              , mScoreB :: Int }
  deriving Show

scoreMatchResult :: ScoreMatch -> MatchResult ()
scoreMatchResult m =
  case mScoreA m `compare` mScoreB m of
    GT -> Home
    EQ -> Tie ()
    LT -> Away

type History = [ScoreMatch]

newtype Tag = Tag { unTag :: String }
  deriving Eq

newtype UefaPureEvaluatorT m a
  = UefaPureEvaluatorT (StateT (Evaluator, History) m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

runUefaPureEvaluatorT (UefaPureEvaluatorT s) eval =
  let (r, (_, h)) = runIdentity $ runStateT s (eval, [])
  in (r, h)

instance Monad m => MonadScoreMatch (UefaPureEvaluatorT m) where
  scoreMatch pa pb = UefaPureEvaluatorT $ do
    (eval, history) <- get
    let (sa, sb) = eval pa pb
        sm = ScoreMatch pa pb sa sb
    modify' (second (++ [sm]))
    pure sm

data TableEntry = TableEntry { tablePoints :: Int
                             , tableMatches :: Int
                             , tableWins :: Int
                             , tableDraws :: Int
                             , tableAwayWins :: Int
                             , tableGoalsFor :: Int
                             , tableGoalsAgainst :: Int
                             , tableAwayGoals :: Int }
  deriving (Show)

tableGoalDifference :: TableEntry -> Int
tableGoalDifference te = tableGoalsFor te - tableGoalsAgainst te

tableLosses te = tableMatches te - (tableWins te + tableDraws te)

onlyWithPlayers :: ScoreMatch -> [Player] -> Bool
onlyWithPlayers ScoreMatch {..} ps =
  mPlayerA `elem` ps && mPlayerB `elem` ps

playerPoints :: Player -> ScoreMatch -> Int
playerPoints p sm
  | p == mPlayerA sm && mScoreA sm  > mScoreB sm = 3
  | p == mPlayerA sm && mScoreA sm == mScoreB sm = 1
  | p == mPlayerA sm && mScoreA sm  < mScoreB sm = 0
  | p == mPlayerB sm && mScoreA sm  < mScoreB sm = 3
  | p == mPlayerB sm && mScoreA sm == mScoreB sm = 1
  | p == mPlayerB sm && mScoreA sm  > mScoreB sm = 0
  | otherwise = error "Player not in match"

playerGoals :: Player -> ScoreMatch -> (Int, Int)
playerGoals p sm
  | p == mPlayerA sm = (mScoreA sm, mScoreB sm)
  | p == mPlayerB sm = (mScoreB sm, mScoreA sm)
  | otherwise = error "Player not in match"

computeScores :: History -> Player -> TableEntry
computeScores hist p =
  let home = filter ((== p) . mPlayerA) hist
      away = filter ((== p) . mPlayerB) hist
      matches = home ++ away
      points = map (playerPoints p) matches
      (for, against) = unzip $ map (playerGoals p) matches
  in
    TableEntry { tablePoints = sum points
               , tableMatches = length matches
               , tableWins = length . filter (== 3) $ points
               , tableDraws = length . filter (== 1) $ points
               , tableAwayWins = length
                                 . filter (\m -> playerPoints p m == 3)
                                 $ away
               , tableGoalsFor = sum for
               , tableGoalsAgainst = sum against
               , tableAwayGoals = sum . map mScoreB $ away }


--uefaSort :: History -> [Player] -> [Player]
uefaSort history =
  concat
  . (sortPoints history >=> tieBreak1 history >=> tieBreak2 history)

sortUsing :: Ord a => (e -> a) -> [Player] -> [e] -> [[Player]]
sortUsing f players entries =
  map (map fst)
  . groupOn snd
  . sortOn snd
  . map (second f)
  $ zip players entries

sortPoints history players =
  let entries = map (computeScores history) players
      f = negate . tablePoints
  in sortUsing f players entries

tieBreak1 :: History -> [Player] -> [[Player]]
tieBreak1 _ [] = []
tieBreak1 _ [x] = [[x]]
tieBreak1 history players =
  let relevantHistory = filter (`onlyWithPlayers` players) history
      entries = map (computeScores relevantHistory) players
      f e = ( negate $ tablePoints e
            , negate $ tableGoalDifference e
            , negate $ tableGoalsFor e
            , negate $ tableAwayGoals e )
  in
    sortUsing f players entries

tieBreak2 :: History -> [Player] -> [[Player]]
tieBreak2 _ [] = []
tieBreak2 _ [x] = [[x]]
tieBreak2 history players =
  let fullEntries = map (computeScores history) players
      f e = ( negate $ tableGoalDifference e
            , negate $ tableGoalsFor e
            , negate $ tableAwayGoals e
            , negate $ tableWins e
            , negate $ tableAwayWins e )
            -- fair play points
            -- former ranking
  in sortUsing f players fullEntries

toTable history players =
  zip players $ map (computeScores history) players

uefaRR4 :: MonadScoreMatch m => Player -> Player -> Player -> Player -> m ()
uefaRR4 p1 p2 p3 p4 =
  mapM_ (uncurry scoreMatch) $ [ (a, b) | a <- [p1, p2, p3, p4]
                                        , b <- [p1, p2, p3, p4]
                                        , a /= b ]

example :: MonadScoreMatch m => m ()
example = uefaRR4 (Player 1) (Player 2) (Player 3) (Player 4)

(_, res) = runUefaPureEvaluatorT example idEval

ps = map Player [1..4]

fakeHist = [ ScoreMatch (Player 1) (Player 2) 1 1
           , ScoreMatch (Player 1) (Player 3) 1 0
           , ScoreMatch (Player 2) (Player 3) 2 0
           , ScoreMatch (Player 1) (Player 4) 1 0
           , ScoreMatch (Player 2) (Player 4) 1 0
           , ScoreMatch (Player 3) (Player 4) 3 2 ]


-- 1. Higher number of points obtained in the matches played among the teams in question;
-- 2. Superior goal difference in matches played among the teams in question;
-- 3. Higher number of goals scored in the matches played among the teams in question;
-- 4. Higher number of goals scored away from home in the matches played among the teams in question;
-- If, after having applied criteria 1 to 4, teams still had an equal ranking, criteria 1 to 4 were reapplied exclusively to the matches between the teams in question to determine their final rankings.[a] If this procedure did not lead to a decision, criteria 6 to 10 applied;
-- 6. Superior goal difference in all group matches;
-- 7. Higher number of goals scored in all group matches;
-- 8. Higher number of away goals scored in all group matches;
-- 9. Higher number of wins in all group matches;
-- 10. Higher number of away wins in all group matches;
-- 11. Lower disciplinary points total in all group matches (1 point for a single yellow card, 3 points for a red card as a consequence of two yellow cards, 3 points for a direct red card, 4 points for a yellow card followed by a direct red card).
-- 12. Position in the 2020–21 UEFA Nations League access list.


-- https://en.wikipedia.org/wiki/2020%E2%80%9321_UEFA_Nations_League_A#Group_3
exampleHistoryGrp3 = [ ScoreMatch (Player 1) (Player 2) 0 0
                     , ScoreMatch (Player 1) (Player 3) 4 2
                     , ScoreMatch (Player 1) (Player 4) 4 2
                     , ScoreMatch (Player 2) (Player 1) 0 1
                     , ScoreMatch (Player 2) (Player 3) 4 1
                     , ScoreMatch (Player 2) (Player 4) 3 0
                     , ScoreMatch (Player 3) (Player 1) 1 2
                     , ScoreMatch (Player 3) (Player 2) 2 3
                     , ScoreMatch (Player 3) (Player 4) 2 1
                     , ScoreMatch (Player 4) (Player 1) 0 1
                     , ScoreMatch (Player 4) (Player 2) 0 2
                     , ScoreMatch (Player 4) (Player 3) 2 1 ]

-- https://en.wikipedia.org/wiki/2020%E2%80%9321_UEFA_Nations_League_A#Group_2
exampleHistoryGrp2 = [ ScoreMatch (Player 1) (Player 2) 4 2
                     , ScoreMatch (Player 1) (Player 3) 2 0
                     , ScoreMatch (Player 1) (Player 4) 5 1
                     , ScoreMatch (Player 2) (Player 1) 0 2
                     , ScoreMatch (Player 2) (Player 3) 0 0
                     , ScoreMatch (Player 2) (Player 4) 2 1
                     , ScoreMatch (Player 3) (Player 1) 2 1
                     , ScoreMatch (Player 3) (Player 2) 0 1
                     , ScoreMatch (Player 3) (Player 4) 4 0
                     , ScoreMatch (Player 4) (Player 1) 1 2
                     , ScoreMatch (Player 4) (Player 2) 0 3
                     , ScoreMatch (Player 4) (Player 3) 0 1 ]

expected = [Player 1, Player 2, Player 3, Player 4]

showTable :: [(Player, TableEntry)] -> String
showTable t = unlines $ header:map showRow (zip [1..] t)
  where w = 3
        showRow (pos, (Player pid, e)) =
          unwords . map (adjustR w . show) $ [ pos
                                             , pid
                                             , tableMatches e
                                             , tableWins e
                                             , tableDraws e
                                             , tableLosses e
                                             , tableGoalsFor e
                                             , tableGoalsAgainst e
                                             , tableGoalDifference e
                                             , tablePoints e ]
        header = unwords . map (adjustR w) $ [ "Pos"
                                             , "PID"
                                             , "Pld"
                                             , "W"
                                             , "D"
                                             , "L"
                                             , "GF"
                                             , "GA"
                                             , "GD"
                                             , "Pts" ]

adjustR n = reverse . adjustL n . reverse
adjustL n s = take n (s ++ repeat ' ')
