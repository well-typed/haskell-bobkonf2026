{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Tutorial where

import Control.Concurrent.STM
import Data.Aeson
import Data.Foldable
import Data.List
import Data.Containers.ListUtils
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ord
import Data.Time
import GHC.Generics
import Web.Scotty

-- PART A. BASICS.
--
-- Main goals:
--
-- - Understand Haskell types and type signatures,
-- - Define and combine simple functions.
--

data Named a = MkNamed { name :: Name, value :: a }
  deriving (Eq, Generic, Show, ToJSON)

type Name = String
type Score = Int

exampleScores :: [Named Score]
exampleScores =
  [ MkNamed "Andres" 17
  , MkNamed "Duncan" 15
  , MkNamed "Andres" 16
  , MkNamed "Edsko" 120
  , MkNamed "Mike"   19
  , MkNamed "Andres" 14
  , MkNamed "Edsko"  88
  ]

-- Exercise A1.
--
-- Find the total score.
-- Use: map, sum
--

totalScore :: [Named Score] -> Score
totalScore scores = sum (map value scores)

-- >>> totalScore exampleScores
-- 289

-- Exercise A2.
--
-- Sort by score (highest first).
-- Use: sortBy, comparing, flip

sortByScore :: [Named Score] -> [Named Score]
sortByScore = sortBy (flip (comparing value))

-- >>> sortByScore exampleScores
-- [MkNamed {name = "Edsko", value = 120},MkNamed {name = "Edsko", value = 88},MkNamed {name = "Mike", value = 19},MkNamed {name = "Andres", value = 17},MkNamed {name = "Andres", value = 16},MkNamed {name = "Duncan", value = 15},MkNamed {name = "Andres", value = 14}]

-- Exercise A3.
--
-- Remove name duplicates and add numeric ranks.
-- Use: nubOrdOn, zip, enumFrom (via range notation)

ranks :: [Named Score] -> [(Int, Named Score)]
ranks = zip [1 ..] . nubOrdOn name . sortByScore

-- >>> ranks exampleScores
-- [(1,MkNamed {name = "Edsko", value = 120}),(2,MkNamed {name = "Mike", value = 19}),(3,MkNamed {name = "Andres", value = 17}),(4,MkNamed {name = "Duncan", value = 15})]

-- Exercise A4.
--
-- Turn everything into a string and print it.
-- Use: show, (<>), unlines, putStr

ranksString :: [Named Score] -> String
ranksString scores = unlines (map (\ (r, (MkNamed n s)) -> show r <> " " <> n <> " (" <> show s <> ")") (ranks scores))

-- >>> ranksString exampleScores
-- "1 Edsko (120)\n2 Mike (19)\n3 Andres (17)\n4 Duncan (15)\n"

printRanks :: [Named Score] -> IO ()
printRanks = putStr . ranksString

-- PART B. WEB SERVICE.
--
-- Main goals:
--
-- - See something happening in the browser.
-- - Get an impression of how to work with "effectful", even concurrent code.

main :: IO ()
main =
  cmdHighScoreService
  -- timedHighScoreService
  -- highScoreService

type HighScoreState = Map Name Score

highScoreService :: IO ()
highScoreService = do
  t <- newTVarIO initialScores
  scotty 8888 (highScoreRoutes t)

highScoreRoutes :: TVar HighScoreState -> ScottyM ()
highScoreRoutes t = do
  get "/scores/query" $ do
    name <- queryParam "name"
    scores <- liftIO (readTVarIO t)
    json (Map.lookup name scores)
  submitScore' t
  topScore t

-- Exercise B1.
--
-- Compute the initial high score map.
-- Use: Map.fromList
-- What will this do with name duplicates?

initialScores :: HighScoreState
initialScores =
  Map.fromList (map (\ (MkNamed n s) -> (n, s)) (nubOrdOn name (sortByScore exampleScores)))

-- Exercise B2.
--
-- Add an endpoint to submit a new score and add it to highScoreRoutes.
-- Use: get/post, atomically, modifyTVar, Map.insert, liftIO

submitScore :: TVar HighScoreState -> ScottyM ()
submitScore t = do
  post "/scores/submit" $ do
    name <- queryParam "name"
    score <- queryParam "score"
    liftIO (atomically (modifyTVar t (Map.insert name score)))
  get "/scores/submit" $ do
    name <- queryParam "name"
    score <- queryParam "score"
    liftIO (atomically (modifyTVar t (Map.insert name score)))

-- Exercise B3.
--
-- Add an endpoint to query for the top score and add it to highScoreRoutes.
-- Return the name and the score (using Named).
-- Use: readTVarIO, Map.toList, NonEmpty.nonEmpty, fmap, maximumBy, snd, json

topScore :: TVar HighScoreState -> ScottyM ()
topScore t = do
  get "/scores/top" $ do
    scores <- liftIO (readTVarIO t)
    json (maxNamedMap scores)

maxNamedMap :: Ord a => Map Name a -> Maybe (Named a)
maxNamedMap m =
      (fmap (uncurry MkNamed . maximumBy (comparing snd))
        (NonEmpty.nonEmpty (Map.toList m))
      )

-- Exercise B4.
--
-- Modify score submission so that it does not overwrite higher scores with
-- lower scores.
-- Use: Map.alter, max / if-then-else / guards / ...

submitScore' :: TVar HighScoreState -> ScottyM ()
submitScore' t = do
  post "/scores/submit" aux
  get "/scores/submit" aux
  where
    aux :: ActionM ()
    aux = do
      name <- queryParam "name"
      score <- queryParam "score"
      liftIO (atomically (modifyTVar t (insertIfHigher name score)))

insertIfHigher :: Name -> Score -> HighScoreState -> HighScoreState
insertIfHigher n s m = Map.alter (max (Just s)) n m
-- NOTE: This makes use of the fact that Nothing is smaller than any Just-constructed
-- value.

-- Exercise B5.
--
-- Tag every score with the current time upon submission.
-- Make all the changes necessary. In the endpoints for querying info,
-- print also the times.
-- Use: getCurrentTime, new datatype for scores with times

data TimedScore = MkTimedScore { score :: Score, time :: UTCTime }
  deriving (Eq, Ord, Generic, Show, ToJSON)

type TimedHighScoreState = Map Name TimedScore

timedHighScoreService :: IO ()
timedHighScoreService = do
  now <- liftIO getCurrentTime
  t <- newTVarIO (fmap (\ score -> MkTimedScore score now) initialScores)
  scotty 8888 (timedHighScoreRoutes t)

timedHighScoreRoutes :: TVar TimedHighScoreState -> ScottyM ()
timedHighScoreRoutes t = do
  get "/scores/query" $ do
    name <- queryParam "name"
    scores <- liftIO (readTVarIO t)
    json (Map.lookup name scores)
  timedSubmitScore t
  timedTopScore t

timedSubmitScore :: TVar TimedHighScoreState -> ScottyM ()
timedSubmitScore t = do
  post "/scores/submit" aux
  get "/scores/submit" aux
  where
    aux :: ActionM ()
    aux = do
      name <- queryParam "name"
      score <- queryParam "score"
      now <- liftIO getCurrentTime
      liftIO (atomically (modifyTVar t (timedInsertIfHigher name (MkTimedScore score now))))

timedInsertIfHigher :: Name -> TimedScore -> TimedHighScoreState -> TimedHighScoreState
timedInsertIfHigher n s m = Map.alter (max (Just s)) n m
-- NOTE: This makes use of the fact that the derived ordering on TimedScore is
-- lexicographical.

timedTopScore :: TVar TimedHighScoreState -> ScottyM ()
timedTopScore t = do
  get "/scores/top" $ do
    scores <- liftIO (readTVarIO t)
    json (maxNamedMap scores)
-- NOTE: This is identical to before, we could also have solved this by generalising
-- the type signature.

-- Exercise B6.
--
-- Other extension ideas:
-- - store all submitted scores per user
-- - add an endpoint to query the top score for each user, ranked
-- - add an endpoint to delete all scores for a given user
-- - ...

-- PART C. ISOLATING THE CORE LOGIC. (if time permits)
--
-- Main goals:
--
-- - Get an idea of the power of the Haskell type system.
-- - Get an idea of how one could write a different interface (or tests) for the same system.

data Command r where
  Submit :: Name -> Score -> Command ()
  Query  :: Name -> Command (Maybe TimedScore)
  Top    :: Command (Maybe (Named TimedScore))

-- Exercise C1.
--
-- Implement a function to run a single command, producing a corresponding
-- result and a new state.
-- (If doing this after B5, you may have to pass in a UTCTime explicitly.)

runCommand :: Command r -> UTCTime -> TimedHighScoreState -> (r, TimedHighScoreState)
runCommand (Submit n s) now scores = ((), timedInsertIfHigher n (MkTimedScore s now) scores)
runCommand (Query n)    _   scores = (Map.lookup n scores, scores)
runCommand Top          _   scores = (maxNamedMap scores, scores)

-- Exercise C2.
--
-- Implement a function to run a single commmand atomically as a side-effecting
-- computation.
-- Use: readTVar, writeTVar

runCommandAtomically :: TVar TimedHighScoreState -> Command r -> IO r
runCommandAtomically t cmd = do
  now <- getCurrentTime
  atomically $ do
    scores <- readTVar t
    let (response, scores') = runCommand cmd now scores
    writeTVar t scores'
    pure response

-- Exercise C3.
--
-- Implement a function to run a command as part of an endpoint, and to
-- display its result in JSON.

runCommandAtomicallyScotty :: ToJSON r => TVar TimedHighScoreState -> Command r -> ActionM ()
runCommandAtomicallyScotty t cmd = do
  response <- liftIO (runCommandAtomically t cmd)
  json response
-- NOTE: This is slightly different from before, because it *always* sends
-- an explicit response.

-- Exercise C4.
--
-- Adapt the routing table (or write a new one) so that it makes use
-- of runCommandAtomicallyScotty.

cmdHighScoreService :: IO ()
cmdHighScoreService = do
  now <- liftIO getCurrentTime
  t <- newTVarIO (fmap (\ score -> MkTimedScore score now) initialScores)
  scotty 8888 (cmdHighScoreRoutes t)

cmdHighScoreRoutes :: TVar TimedHighScoreState -> ScottyM ()
cmdHighScoreRoutes t = do
  get "/scores/query" $ do
    name <- queryParam "name"
    runCommandAtomicallyScotty t (Query name)
  post "/scores/submit" $ do
    name <- queryParam "name"
    score <- queryParam "score"
    runCommandAtomicallyScotty t (Submit name score)
  get "/scores/submit" $ do
    name <- queryParam "name"
    score <- queryParam "score"
    runCommandAtomicallyScotty t (Submit name score)
  get "/scores/top" $ do
    runCommandAtomicallyScotty t Top

-- Exercise C5.
--
-- Adapt this new setup to other extensions made in Part 2 (current time,
-- more commands, ...).

-- (Integrated above.)

-- Exercise C6.
--
-- There are several wider ideas for extending this setup, for example:
-- - write a CLI-based user interface,
-- - think about testing the core logic,
-- - ...
