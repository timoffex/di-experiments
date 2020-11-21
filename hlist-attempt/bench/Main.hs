module Main where

import qualified Bench.Examples.Life
import           Criterion.Main

main = defaultMain
  [ Bench.Examples.Life.benchmark
  ]
