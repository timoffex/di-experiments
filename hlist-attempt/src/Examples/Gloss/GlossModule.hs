{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}



module Examples.Gloss.GlossModule
  ( glossModule
  , playGame

  , ViewTransform
  , translation
  , scaling

  , addWorldState, World
  , addUpdateHook, addUpdateHook', UpdateHook
  , addEventHook, addEventHook', EventHook
  , addRenderHook, addRenderHook', RenderHook

  -- * Re-exports of Gloss bindings for convenience
  , module Examples.Gloss.GlossUtils
  ) where


import           Control.DI
import           Control.Lens
import           Examples.Gloss.GlossUtils
import           Graphics.Gloss.Interface.Pure.Game



data ViewTransform = ViewTransform
  { _viewTransformTranslation :: (Float, Float)
  -- ^ Translation applied to the rendered picture.
  , _viewTransformScaling     :: (Float, Float)
  -- ^ Scaling applied to the rendered picture.
  --
  -- Scaling is applied after translation.
  }
$( makeFields ''ViewTransform )

defaultViewTransform :: ViewTransform
defaultViewTransform = ViewTransform (0, 0) (1, 1)



-- | Installs the base bindings for using 'playGame'.
--
-- The initial world has a 'ViewTransform' component.
glossModule :: forall all ins w.
               ( w ~ World (FirstTagged World all)
               ) => Module all ins _
glossModule =
  bind (World (HCons defaultViewTransform HEmpty))
  -- The hook types must be specified here so that the compiler can
  -- infer concrete types even if the hooks are unused. Without these
  -- annotations combined with @w ~ World (FirstTagged World all)@,
  -- the compiler will complain about ambiguous types if this module
  -- is installed but 'playGame' isn't used.
  --
  -- In the case of 'glossModule', it doesn't make much sense to use
  -- it without 'playGame'. One use case might be to temporarily try
  -- something other than 'playGame' without having to comment out the
  -- module. I mainly write these type annotations here to set a good
  -- example.
  `install` bind (UpdateHook $ const id     :: UpdateHook     w)
  `install` bind (RenderHook $ const blank  :: RenderHook     w)
  `install` bind (EventHook  $ const id     :: EventHook      w)


-- | Uses 'gloss' to play a game in a new window.
--
-- The game's initial state is defined by the 'World' binding. Use
-- 'addWorldState' to add extra state to the game. You can operate on
-- any added state by registering hooks using 'addUpdateHook',
-- 'addEventHook' or 'addRenderHook'.
playGame :: _ => HList deps -> IO ()
playGame deps = let initialWorld   = World $ fromWorld $ hget1 deps
                    updateHook     = fromUpdateHook $ hget1 deps
                    eventHook      = fromEventHook $ hget1 deps
                    renderHook     = fromRenderHook $ hget1 deps
                in play
                     (InWindow "Game" (800, 600) (0, 0))
                     white
                     2
                     initialWorld
                     (\w -> let transform = hget (fromWorld w) :: ViewTransform
                                (sx, sy) = transform^.scaling
                                (tx, ty) = transform^.translation
                            in scale sx sy
                               $ translate tx ty
                               $ renderHook w)
                     eventHook
                     updateHook

-- Ensures that if the module compiles, then the types for glossModule
-- and playGame actually match up.
_test_ = inject glossModule playGame


-- | Add state to the world.
addWorldState :: ( CanReplace1 World xs (s ': xs) ins
                 , xs ~ FirstTagged World ins
                 )
              => s -> Module all ins (Replace1 World (s ': xs) ins)
addWorldState s = replace1 $ underWorld (HCons s)


-- | Registers an update hook that acts on the 's' in the 'World' state.
--
-- Hooks are applied in the order they are registered.
addUpdateHook :: _ => (Float -> s -> s) -> Module all ins ins
addUpdateHook hook = addUpdateHook' $ \dt -> hmap $ hook dt

-- | Registers an update hook that acts on the 'World' state.
--
-- Hooks are applied in the order they are registered.
addUpdateHook' :: _ => (Float -> HList xs -> HList xs) -> Module all ins ins
addUpdateHook' hook = replace1 $ \currentHook -> UpdateHook $ \dt ->
  underWorld (hook dt) . fromUpdateHook currentHook dt


-- | Registers an event hook that updates the 's' from the 'World' state.
--
-- Hooks are applied in the order they are registered.
addEventHook :: _ => (Event -> s -> s) -> Module all ins ins
addEventHook hook = addEventHook' $ \evt -> hmap $ hook evt

-- | Registers an event hook that updates the 'World'.
--
-- Hooks are applied in the order they are registered.
addEventHook' :: _ => (Event -> HList xs -> HList xs) -> Module all ins ins
addEventHook' hook = replace1 $ \currentHook -> EventHook $ \evt ->
  underWorld (hook evt) . fromEventHook currentHook evt


-- | Registers a render hook that renders the 's' from the 'World' state.
--
-- By convention, the rendered picture should be in world coordinates.
-- The final picture is transformed according to the 'ViewTransform'
-- component of the world state. The pictures output by later hooks
-- are overlaid on top of the outputs of earlier hooks.
addRenderHook :: _ => (s -> Picture) -> Module all ins ins
addRenderHook hook = addRenderHook' $ hook . hget

-- | Registers a render hook that renders a picture based on the
-- 'World' state.
--
-- See 'addRenderHook''.
addRenderHook' :: _ => (HList xs -> Picture) -> Module all ins ins
addRenderHook' hook = replace1 $ \currentHook -> RenderHook $ \w ->
  fromRenderHook currentHook w <> hook (fromWorld w)


-- | The state used in the simulation.
--
-- Modules should use 'addWorldState' to register extra state.
newtype World xs = World { fromWorld :: HList xs }
underWorld :: (HList xs -> HList ys) -> World xs -> World ys
underWorld f = World . f . fromWorld


-- | A hook for updating the simulation state.
--
-- The function accepts a 'Float' argument which is the number of
-- seconds since the last update, and then updates the world 'w'.
--
-- Add hooks using 'addUpdateHook'.
newtype UpdateHook w = UpdateHook { fromUpdateHook :: Float -> w -> w }

-- | A hook for parsing events.
--
-- The function updates a world 'w' given an 'Event'.
--
-- Add hooks using 'addEventHook'.
newtype EventHook w = EventHook { fromEventHook :: Event -> w -> w }

-- | A hook for computing a picture from the simulation state.
--
-- The function accepts a 'w' and outputs a 'Picture' to display. By
-- convention, the 'Picture' is in world coordinates. Use
-- 'PostRenderHook' to apply transformations.
--
-- Add hooks using 'addRenderHook'.
newtype RenderHook w = RenderHook { fromRenderHook :: w -> Picture }
