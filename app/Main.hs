-- My IDE fails to work on this file because the types are too
-- complicated.

module Main where

import           Control.DI
import           Examples.Gloss.BasicViewControlsModule
import           Examples.Gloss.GlossModule
import           Examples.Life.LifeGlossModule
import           Examples.Life.LifeGrid


main :: IO ()
main = inject mainModule playGame

mainModule =
  glossModule
  `install` basicViewControlsModule
  `install` lifeGlossModule initialWorld 10


initialWorld = fromList
  [
  -- A plane
    (0, 0)
  , (1, 0)
  , (2, 0)
  , (2, 1)
  , (1, 2)

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
