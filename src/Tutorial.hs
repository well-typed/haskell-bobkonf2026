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
-- Remove duplicates and add numeric ranks.
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

-- Exercise B1.
--
-- Compute the initial high score map.
-- Use: Map.fromList, map, anonymous function

initialScores :: HighScoreState
initialScores = error "implement me"

-- Exercise B2.
--
-- Add an endpoint to submit a new score and add it to highScoreRoutes.
-- Use: get/post, atomically, modifyTVar, Map.insert

-- Exercise B3.
--
-- Add an endpoint to query for the top score and add it to highScoreRoutes.

-- Exercise B4.
--
-- Modify score submission so that it does not overwrite higher scores with
-- lower scores.

-- Exercise B5.
--
-- Tag every score with the current time. In the endpoints for querying info,
-- print also the times.
--
-- Use: getCurrentTime, new datatype for scores with times

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
  Query  :: Name -> Command (Maybe Score) -- possibly change the type here as a result of Ex. B5

-- Exercise C1.
--
-- Implement a function to run a single command, producing a corresponding
-- result and a new state.

runCommand :: Command r -> HighScoreState -> (r, HighScoreState)
runCommand = error "implement me"

-- Exercise C2.
--
-- Implement a function to run a single commmand atomically as a side-effecting
-- computation.
--
-- Use: readTVar, writeTVar

runCommandAtomically :: TVar HighScoreState -> Command r -> IO r
runCommandAtomically = error "impement me"

-- Exercise C3.
--
-- Implement a function to run a command as part of an endpoint, and to
-- display its result in JSON.

runCommandAtomicallyScotty :: ToJSON r => TVar HighScoreState -> Command r -> ActionM ()
runCommandAtomicallyScotty = error "implement me"

-- Exercise C4.
--
-- Adapt the routing table (or write a new one) so that it makes use
-- of runCommandAtomicallyScotty.

-- Exercise C5.
--
-- Adapt this new setup to other extensions made in Part 2 (current time,
-- more commands, ...).

-- Exercise C6.
--
-- There are several wider ideas for extending this setup, for example:
-- - write a CLI-based user interface,
-- - think about testing the core logic,
-- - ...
