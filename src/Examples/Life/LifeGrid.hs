{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Examples.Life.LifeGrid
  ( LifeGrid
  , advance
  -- * Construction
  , emptyWorld
  , fromList
  -- * Modification
  , kill
  , vitalize
  , setAlive
  , setAliveAt
  -- * Querying
  , cellAt
  , alivePositions
  , isAliveAt
  -- * Utility
  , neighbors
  ) where

import           Control.DeepSeq     (NFData)
import           Control.Lens
import           Control.Monad
import qualified Data.HashMap.Strict as M
import           Data.Maybe


newtype NeighborCount = NeighborCount
  { _neighborCountValue :: Int
  } deriving (NFData)
newtype CellAlive =CellAlive
  { _cellAliveValue :: Bool
  } deriving (NFData)

type Cell = (NeighborCount, CellAlive)
newtype LifeGrid = LifeGrid
  { _lifeGridGridMap :: M.HashMap (Int, Int) Cell
  } deriving (NFData)

$( makeFields ''NeighborCount )
$( makeFields ''CellAlive )
$( makeFields ''LifeGrid )


-- | Lens for the cell at the position.
cellAt :: (Int, Int) -> Lens' LifeGrid Bool
cellAt xy = lens (isAliveAt xy) (flip $ setAliveAt xy)


checkAlive :: Cell -> Bool
checkAlive = view $ _2 . value


-- | Gets the positions of all living cells, in no specified order.
alivePositions :: LifeGrid -> [(Int, Int)]
alivePositions = toListOf $ gridMap . ifolded . filtered checkAlive . asIndex


-- | Creates a 'LifeGrid' given the positions of the living cells.
fromList :: [(Int, Int)] -> LifeGrid
fromList = foldl (flip vitalize) emptyWorld


-- | The 'LifeGrid' with no living cells.
emptyWorld :: LifeGrid
emptyWorld = LifeGrid M.empty


-- | Checks whether the cell at the position is alive.
isAliveAt :: (Int, Int) -> LifeGrid -> Bool
isAliveAt xy lg = fromMaybe False (lg ^? gridMap . at xy . _Just . _2 . value)

-- | Checks whether the cell at the position is alive.
isAliveIn :: LifeGrid -> (Int, Int) -> Bool
isAliveIn = flip isAliveAt

-- | Either vitalizes or kills the cell at the position.
--
-- @
--   setAliveAt = flip setAlive
-- @
setAliveAt :: (Int, Int) -> Bool -> LifeGrid -> LifeGrid
setAliveAt = flip setAlive


-- | Either vitalizes or kills the cell at the position.
setAlive :: Bool -> (Int, Int) -> LifeGrid -> LifeGrid
setAlive b pos prevLg =
  if b == prevLg^.cellAt pos
    then prevLg
    else foldl adjustNeighbor lgWithPosSet posNeighbors
  where
    posNeighbors  = neighbors pos

    -- Number of living neighbors of 'pos' in 'prevLg'.
    neighborCount = length $ filter id $ isAliveIn prevLg <$> posNeighbors

    lgWithPosSet = prevLg & gridMap.at pos %~ \case
      -- Create new cell if one doesn't exist and b is True.
      Nothing ->
        if b
          then Just (NeighborCount neighborCount, CellAlive True)
          else Nothing
      -- Otherwise just change its 'alive' status, removing it if it
      -- is a dead cell with no neighbors.
      Just (NeighborCount nc, _) ->
        if not b && (nc == 0)
          then Nothing
          else Just (NeighborCount nc, CellAlive b)

    adjustNeighbor lg pos = lg & gridMap.at pos %~ if b
                                                      then incNeighbor
                                                      else decNeighbor

    incNeighbor Nothing                       = Just (NeighborCount 1, CellAlive False)
    incNeighbor (Just (NeighborCount nc, ca)) = Just (NeighborCount (nc + 1), ca)

    decNeighbor Nothing = error "All neighbors of a living cell have a neighbor."

    -- Remove cells that aren't alive and have no neighbors.
    decNeighbor (Just (NeighborCount 1, CellAlive False)) = Nothing
    -- Otherwise just decrease the neighbor count.
    decNeighbor (Just (NeighborCount nc, ca)) = Just (NeighborCount (nc - 1), ca)

-- | Marks the cell at the position as alive.
vitalize :: (Int, Int) -> LifeGrid -> LifeGrid
vitalize = setAlive True


-- | Kills the cell at the position.
kill :: (Int, Int) -> LifeGrid -> LifeGrid
kill = setAlive False


-- | Advances the 'LifeGrid' according to the "B3/S23" rules.
--
-- B3/S23 means a cell comes to life if it has exactly 3 alive
-- neighbors, survives if it has either 2 or 3 alive neighbors, and
-- dies otherwise.
advance :: LifeGrid -> LifeGrid
advance prevLg = foldl stepCell prevLg (prevLg^..gridMap.itraversed.withIndex)
  where
    stepCell = flip stepCell'
    stepCell' (pos, (NeighborCount nc, CellAlive True))
      -- Living cells with 2 or 3 neighbors survive.
      | nc == 2 || nc == 3   = id
      -- Otherwise they die.
      | otherwise            = kill pos

    -- Non-living cells with 3 neighbors come to life.
    stepCell' (pos, (NeighborCount 3, CellAlive False)) =
      vitalize pos

    -- Otherwise nothing changes.
    stepCell' _ = id



-- | Gets the 8 neighboring positions of a position.
neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) =
  [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
  , (x - 1, y    ),             (x + 1, y    )
  , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
  ]
