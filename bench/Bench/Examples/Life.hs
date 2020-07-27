module Bench.Examples.Life
  ( benchmark
  ) where

import           Criterion.Main
import           Examples.Life.LifeGrid


-- | An initial 'LifeGrid' for benchmarking purposes.
--
-- This stays alive for a while, but I have not yet measured for how
-- long (TODO).
initialBenchWorld = fromList
  [
  -- A plane
    (0, 0)
  , (1, 0)
  , (2, 0)
  , (2, 1)
  , (1, 2)

  -- An extra cell that makes the world interesting
  , (3, 0)

  -- A spinny thing
  , (4, 5)
  , (5, 5)
  , (6, 5)

  -- A block
  , (-3, -3)
  , (-2, -3)
  , (-2, -2)
  , (-3, -2)
  ]


advanceN 0 lg = lg
advanceN n lg = advanceN (n - 1) $ advance lg

benchmark = bgroup "Conway's Game of Life"
  [ bench "10 steps" $ nf (advanceN 10) initialBenchWorld
  , bench "50 steps" $ nf (advanceN 50) initialBenchWorld
  , bench "100 steps" $ nf (advanceN 100) initialBenchWorld
  ]
