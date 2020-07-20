{-# LANGUAGE TemplateHaskell #-}

-- | Generates prisms and lenses for various Gloss types.
--
-- Import 'Examples.Gloss.GlossUtils' instead of this.
module Examples.Gloss.GlossGenerated where

import           Control.Lens
import           Graphics.Gloss.Interface.Pure.Game

$( makePrisms ''Event )
$( makePrisms ''MouseButton )
$( makePrisms ''Key )
$( makePrisms ''SpecialKey )
$( makePrisms ''KeyState )
