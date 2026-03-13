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

-- PART 1. BASICS.
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

-- Exercise 1.
--
-- Find the total score.
-- Use: map, sum
--

totalScore :: [Named Score] -> Score
totalScore = error "implement me"

-- Exercise 2.
--
-- Find the number of duplicates.
-- Use: sort, group, length, sum

countDuplicates :: [Named Score] -> Int
countDuplicates = error "implement me"

-- Exercise 3.
--
-- Sort by score.
--
-- Use: sortBy, comparing

sortByScore :: [Named Score] -> [Named Score]
sortByScore = error "implement me"

-- Exercise 4.
--
-- Remove duplicates and add numeric ranks.
-- Use: nubOrd, zip, enumFrom (via range notation)

ranks :: [Named Score] -> [(Int, Named Score)]
ranks = error "implement me"

-- Exercise 5.
--
-- Turn everything into a string and print it.
-- Use: show, (<>), unlines, putStr

ranksString :: [Named Score] -> String
ranksString = error "implement me"

printRanks :: [Named Score] -> IO ()
printRanks = error "implement me"

-- PART 2. WEB SERVICE.
--
-- Main goals:
--
-- - See something happening in the browser.
-- - Get an impression of how to work with "effectful", even concurrent code.

main :: IO ()
main = highScoreService

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

-- Exercise 6.
--
-- Compute the initial high score map.
-- Use: Map.fromList, map, anonymous function

initialScores :: HighScoreState
initialScores = error "implement me"

-- Exercise 7.
--
-- Add an endpoint to submit a new score and add it to highScoreRoutes.
-- Use: get/post, atomically, modifyTVar, Map.insert

-- Exercise 8.
--
-- Add an endpoint to query for the top score and add it to highScoreRoutes.

-- Exercise 9.
--
-- Modify score submission so that it does not overwrite higher scores with
-- lower scores.

-- Exercise 10.
--
-- Tag every score with the current time. In the endpoints for querying info,
-- print also the times.
--
-- Use: getCurrentTime, new datatype for scores with times

-- Exercise 11.
--
-- Other extension ideas:
-- - store all submitted scores per user
-- - add an endpoint to query the top score for each user, ranked
-- - add an endpoint to delete all scores for a given user
-- - ...

-- PART 3. ISOLATING THE CORE LOGIC. (if time permits)
--
-- Main goals:
--
-- - Get an idea of the power of the Haskell type system.
-- - Get an idea of how one could write a different interface (or tests) for the same system.

data Command r where
  Submit :: Name -> Score -> Command ()
  Query  :: Name -> Command (Maybe Score) -- possibly change the type here as a result of Ex. 10

-- Exercise 12.
--
-- Implement a function to run a single command, producing a corresponding
-- result and a new state.

runCommand :: Command r -> HighScoreState -> (r, HighScoreState)
runCommand = error "implement me"

-- Exercise 13.
--
-- Implement a function to run a single commmand atomically as a side-effecting
-- computation.
--
-- Use: readTVar, writeTVar

runCommandAtomically :: TVar HighScoreState -> Command r -> IO r
runCommandAtomically = error "impement me"

-- Exercise 14.
--
-- Implement a function to run a command as part of an endpoint, and to
-- display its result in JSON.

runCommandAtomicallyScotty :: ToJSON r => TVar HighScoreState -> Command r -> ActionM ()
runCommandAtomicallyScotty = error "implement me"

-- Exercise 15.
--
-- Adapt the routing table (or write a new one) so that it makes use
-- of runCommandAtomicallyScotty.

-- Exercise 16.
--
-- Adapt this new setup to other extensions made in Part 2 (current time,
-- more commands, ...).

-- Exercise 17.
--
-- There are several wider ideas for extending this setup, for example:
-- - write a CLI-based user interface,
-- - think about testing the core logic,
-- - ...
