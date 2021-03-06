{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Defines nice default controls to use with
-- 'Examples.Gloss.GlossModule'.
module Examples.Gloss.BasicViewControlsModule
  ( basicViewControlsModule
  ) where


import           Control.Arrow              ((***), (>>>))
import           Control.DI
import           Control.Lens
import           Control.Lens.Extra
import           Control.Monad.Extra
import           Control.Monad.State
import           Examples.Gloss.GlossModule


data MouseState = MouseState
  { _mouseStateLastXY      :: (Float, Float)
    -- ^ The last observed (X, Y) position of the mouse.
  , _mouseStateIsLeftDown  :: Bool
    -- ^ Whether the left mouse button is down.
    --
    -- TODO: This isn't used here. Move mouse stuff to a separate
    -- module, and then this might be useful.
  , _mouseStateIsRightDown :: Bool
    -- ^ Whether the right mouse button is down.
  }
$( makeFields ''MouseState )
initialMouseState = MouseState (0, 0) False False


-- | Installs hooks to pan and zoom the final picture.
--
-- This is meant to be installed on top of
-- 'Examples.Gloss.GlossModule'. This modifies the 'ViewTransform'
-- component.
--
-- Panning is done by holding down and dragging the right mouse
-- button. TODO: Zooming is not yet implemented.
basicViewControlsModule :: _ => Module all ins _
basicViewControlsModule =
  addWorldState initialMouseState
  `install` addEventHook' handleEvent


handleEvent :: _ => Event -> HList xs -> HList xs
handleEvent evt = execState $ do
  -- Update button-down state.
  whenJust (evt ^? (_EventKey . aside1 _MouseButton)) $ \(button, ks, _, _) ->
    case button of
      RightButton -> hlift @MouseState isRightDown .= (ks == Down)
      LeftButton  -> hlift @MouseState isLeftDown  .= (ks == Down)
      _           -> return ()

  -- Handle mouse motion.
  case evt of
    EventKey _ _ _ mp -> handleMouseMoved mp
    EventMotion    mp -> handleMouseMoved mp
    _                 -> return ()



handleMouseMoved :: _ => (Float, Float) -> StateT (HList xs) m ()
handleMouseMoved (nx, ny) = do
  (lx, ly) <- use (hlift @MouseState lastXY)

  let dx = lx - nx
      dy = ly - ny

  whenM (use (hlift @MouseState isRightDown)) $
    hlift @ViewTransform translation %= (subtract dx *** subtract dy)

  hlift @MouseState lastXY .= (nx, ny)
