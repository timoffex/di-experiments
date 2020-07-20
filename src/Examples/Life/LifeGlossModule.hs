{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Shows a simulation of Conway's Game of Life using Gloss.
module Examples.Life.LifeGlossModule
  ( lifeGlossModule
  ) where


import           Control.Arrow
import           Control.DI
import           Control.Lens
import           Control.Monad.Extra         (whenM)
import           Control.Monad.State
import           Examples.Gloss.GlossModule
import           Examples.Life.LifeGrid
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Data.Picture


newtype MouseState = MouseState
  { _mouseStateIsLeftDown :: Bool }
$( makeFields ''MouseState )

initialMouseState :: MouseState
initialMouseState = MouseState False


newtype SimulationState = SimulationState
  { _simulationStateIsSimulating :: Bool }
$( makeFields ''SimulationState )

initialSimulationState :: SimulationState
initialSimulationState = SimulationState False


-- | Simulates Conway's Game of Life.
--
-- This depends on the 'glossModule'. This installs a 'LifeGrid'
-- component, an update hook that advances the 'LifeGrid', a render
-- hook that draws the 'LifeGrid' with cells of size @cellScale@, and
-- (TODO) an event hook that allows editing the grid and pausing or
-- stopping the simulation.
lifeGlossModule :: _ => LifeGrid -> Float -> Module all ins _
lifeGlossModule initialWorld cellScale =
  addWorldState initialWorld
  `install` addWorldState initialMouseState
  `install` addWorldState initialSimulationState
  `install` addUpdateHook' (const onUpdate)
  `install` addRenderHook (drawLife cellScale)
  `install` addEventHook' (onEvent cellScale)


drawLife cellScale lg = pictures $ drawCell cellScale <$> alivePositions lg
drawCell cellScale (x, y) =
  translate (cellScale * fromIntegral x) (cellScale * fromIntegral y)
  $ color black
  $ rectangleSolid cellScale cellScale


-- This trivial partial type signature is necessary, or else the
-- compiler complains about ambiguous types. Weird!
onUpdate :: _ => _
onUpdate = execState $
  whenM (gets $ hget >>> view (isSimulating @SimulationState)) $
    modify $ hmap advance

onEvent cellSize evt = case evt of
  EventKey (MouseButton LeftButton) Down _ mousePos -> execState $ do
    modify $ hmap $ set (isLeftDown @MouseState) True
    vitalizeMouse cellSize mousePos

  EventMotion mousePos -> execState $
    whenM (gets $ hget >>> view (isLeftDown @MouseState)) $
      vitalizeMouse cellSize mousePos

  EventKey (MouseButton LeftButton) Up _ _ ->
    hmap $ set (isLeftDown @MouseState) False

  EventKey (SpecialKey KeySpace) Up _ _ ->
    hmap $ over (isSimulating @SimulationState) not

  _ -> id

vitalizeMouse cellSize (mx, my) = do
    (viewTransform :: ViewTransform) <- gets hget
    let (tx, ty) = viewTransform^.translation
        (sx, sy) = viewTransform^.scaling
        wx = round $ (mx - tx) / (sx * cellSize)
        wy = round $ (my - ty) / (sy * cellSize)
    modify $ hmap $ vitalize (wx, wy)
