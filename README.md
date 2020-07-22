# Dependency injection "framework" in Haskell

## Overview

This is a small project where I make Haskell's type inference do the
work that dependency injection frameworks do in other languages.

The dependency injection API is implemented in the `Control.DI`
module, found in `src/Control/DI.hs`. This is a very small file: 15
non-blank, non-comment, non-header lines of code, with 133 lines total
at the time of writing.

The bulk of the work went into the `Data.HList` module which
implements utilities for working with "heterogeneous lists", which are
also known as "tuples" except that Haskell's actual tuples are not
recursively defined (a tuple isn't an element + a tuple one size
smaller) and therefore are difficult to use in type-level operations.
There is an `HList` package on hackage which is significantly more
mature and principled, but I wrote my own for these reasons:

- To get more experience working with Haskell's advanced type
  features. I learned a lot about debugging type ambiguity compilation
  errors throughout this project.
- To get a clear understanding of exactly what functionality is
  actually necessary for the purpose of DI.

## Example implementation of Conway's Game of Life

To get a sense of usability, I included an example implementation of
Conway's Game of Life using this DI "framework". The actual game logic
is implemented in `src/Examples/Life/LifeGrid.hs` without DI. DI is
used to put it together with the vector graphics package `gloss` in a
way that cleanly separates various concerns.

`Examples.Gloss.GlossModule` defines the base DI module for using
Gloss: it installs initial bindings for the world state and various
hooks and defines the `playGame` function. It defines the concept of
the view transform (panning & zooming) but expects a separate module
to define how panning and zooming should work.

`Examples.Gloss.BasicViewControlsModule` implements panning. It
registers some extra state to keep track of the mouse and adds an
event hook that pans the view when the user drags with the right mouse
button.

`Examples.Life.LifeGlossModule` hooks the game of life into Gloss. It
registers extra state including a `LifeGrid` and some stuff to keep
track of whether the simulation is paused and whether the left mouse
button is down (it does not reuse the mouse data from
`BasicViewControlsModule`; ideally there would be a separate
`MouseModule` that handles all things mouse related). It adds an event
hook to allow the user to paint new cells and pause or resume the
simulation. It adds an update hook to advance the `LifeGrid` when the
simulation is running, and it adds a render hook to draw the
`LifeGrid`.

As of writing, `app/Main.hs` installs all three modules and then
injects and runs `playGame`.

## What does it do?

Consider this outline of a Haskell implementation of a game:

```haskell
playGame :: w                  -- ^ Initial game state ("world").
         -> (Float -> w -> w)  -- ^ Update function.
         -> (Event -> w -> w)  -- ^ Event handler.
         -> (w -> Picture)     -- ^ Rendering function.
         -> IO ()


initialLifeGrid :: LifeGrid
updateLifeGame :: LifeGrid -> LifeGrid
renderLifeGame :: LifeGrid -> Picture
handlePainting :: Event -> MouseState -> LifeGrid -> LifeGrid

initialSimulationState :: SimulationState
updateSimulationState :: Event -> SimulationState -> SimulationState

initialMouseState :: MouseState
updateMouse :: Event -> MouseState -> MouseState

initialTransform :: Transform
handlePanning :: Event -> MouseState -> Transform -> Transform


main =
  playGame
    ( initialLifeGrid
    , initialSimulationState
    , initialMouseState
    , initialTransform )
    
    (\dt (lg, ss, ms, tr) ->
      ( updateLifeGame lg, ss, ms, tr ))
    
    (\evt (lg, ss, ms, tr) ->
      let ms' = updateMouse evt ms
          ss' = updateSimulationState evt ss
          tr' = handlePanning evt ms tr
          lg' = handlePainting evt ms lg
      in (lg', ss', ms', tr'))
    
    (\(lg, ss, ms, tr) -> renderLifeGame lg)
```

One can separate the functions and data definitions (omitted) into
different files, grouping by logical similarity. The functions acting
on a `LifeGrid` might go in one file, the functions for controls might
go in another.

Regardless of how one distributes the lower-level definitions across
files, everything is eventually imported into the main file for use by
the `main` action. Because of this, even small changes to the game
require touching the main file in several places. For example:

- To render a cursor, you will need to define a `renderCursor`
  function and insert it into the correct place in `main`. You will
  probably add extra fields to `MouseState`, but fortunately that will
  not affect `main`.

- To add a UI, you will need extra state `initialUiState`, a new event
  handler `handleUiEvent` and a new rendering function
  `renderUi`. These must be inserted in three different spots in
  `main`. Moreover, since you're adding a new element to the state
  tuple, *you must update every place that depends on the tuple
  structure*, so every instance of `(lg, ss, ms, tr)` must change to
  `(lg, ss, ms, tr, ui)`. Ouch!
  
If you are working on this code with a team of developers, you will
run into frequent merge conflicts because commits will often make
changes to this one file.

Not only does this design complicate writing code, it also makes
reading the code more difficult. The `main` action has too much
detail: it describes the way state is ordered and how it's used.
Ideally, it would just give an overview of what the game consists of:
it's Conway's Game of Life with basic controls. If you factor `main`
into helper functions, the situation only becomes worse:

```haskell
-- To understand what this is, you now have to read through
-- four helper functions. To add functionality, you'll now have
-- to touch 4 separate functions instead of just main.
main = playGame initialState onUpdate onEvent render
```

Ideally, you would define the program like so:

- Separate the program into "modules" that define small,
  self-contained pieces of data and functionality. You might have a
  module for implementing view-related controls like panning and a
  module for implementing Conway's Game of Life (game logic,
  event-handling for painting cells, rendering logic).

- In `main`, just declare which modules are used, without explicitly
  wiring them together.

It turns out that the main challenge of doing this is avoiding the
explicit "wiring" in `main`: the `(lg, ss, ms, tr)` bits, the ordering
of the functions in the event handler, the awareness of which modules
declare a rendering function and which do not. This is what dependency
injection frameworks are used for: automatically wiring together
pieces of the program that use a declarative syntax to describe their
dependencies and outputs.

In languages like Java and C#, dependency injection frameworks use
reflection or code generation to get rid of the wiring boilerplate. It
turns out that Haskell's type system is powerful enough to do that on
its own:

```haskell
main = inject mainModule playGame

mainModule =
  baseGlossModule
  `install` basicViewControlsModule
  `install` lifeGlossModule

-- GlossModule.hs
glossModule :: forall all ins w.
               ( w ~ World (FirstTagged World all)
               ) => Module all ins _
glossModule =
  -- Defines the basic game state necessary for running the game.
  bind (World (HCons defaultViewTransform HEmpty))
  -- Declares hooks. In-line type signatures are necessary in this
  -- one spot for a technical reason (see GlossModule.hs).
  `install` bind (UpdateHook $ const id     :: UpdateHook     w)
  `install` bind (RenderHook $ const blank  :: RenderHook     w)
  `install` bind (EventHook  $ const id     :: EventHook      w)

-- Also defines the 'playGame' function.

-- BasicViewControlsModule.hs
basicViewControlsModule =
  addWorldState initialMouseState
  `install` addEventHook' handleEvent

-- LifeGlossModule.hs
lifeGlossModule initialWorld cellScale =
  addWorldState initialWorld
  `install` addWorldState initialMouseState
  `install` addWorldState initialSimulationState
  `install` addUpdateHook' (const onUpdate)
  `install` addRenderHook (drawLife cellScale)
  `install` addEventHook' (onEvent cellScale)

```

In the code above, I intentionally omit type signatures in most
places. This is because the type signature of every module includes
its transitive outputs and transitive dependencies. These are fully
inferred by Haskell. Here is the full, inferred type of `mainModule`:

```haskell
mainModule
  :: Module
       '[EventHook
           (World
              '[Examples.Life.LifeGlossModule.SimulationState,
                Examples.Life.LifeGlossModule.MouseState, LifeGrid,
                Examples.Gloss.BasicViewControlsModule.MouseState, ViewTransform]),
         RenderHook
           (World
              '[Examples.Life.LifeGlossModule.SimulationState,
                Examples.Life.LifeGlossModule.MouseState, LifeGrid,
                Examples.Gloss.BasicViewControlsModule.MouseState, ViewTransform]),
         UpdateHook
           (World
              '[Examples.Life.LifeGlossModule.SimulationState,
                Examples.Life.LifeGlossModule.MouseState, LifeGrid,
                Examples.Gloss.BasicViewControlsModule.MouseState, ViewTransform]),
         World
           '[Examples.Life.LifeGlossModule.SimulationState,
             Examples.Life.LifeGlossModule.MouseState, LifeGrid,
             Examples.Gloss.BasicViewControlsModule.MouseState, ViewTransform]]
       ins
	   -- This is the same as above. It appears twice because of the
	   -- definition of the 'Module' type and the 'inject' function.
	   -- Without getting into technicalities, this makes it possible to
	   -- declare hooks that act on the "total" state, which may be
	   -- composed of bits of state defined in unknown modules.
       (EventHook
          (World
             '[Examples.Life.LifeGlossModule.SimulationState,
               Examples.Life.LifeGlossModule.MouseState, LifeGrid,
               Examples.Gloss.BasicViewControlsModule.MouseState, ViewTransform])
          : RenderHook
              (World
                 '[Examples.Life.LifeGlossModule.SimulationState,
                   Examples.Life.LifeGlossModule.MouseState, LifeGrid,
                   Examples.Gloss.BasicViewControlsModule.MouseState, ViewTransform])
          : UpdateHook
              (World
                 '[Examples.Life.LifeGlossModule.SimulationState,
                   Examples.Life.LifeGlossModule.MouseState, LifeGrid,
                   Examples.Gloss.BasicViewControlsModule.MouseState, ViewTransform])
          : World
              '[Examples.Life.LifeGlossModule.SimulationState,
                Examples.Life.LifeGlossModule.MouseState, LifeGrid,
                Examples.Gloss.BasicViewControlsModule.MouseState, ViewTransform]
          : ins)
```
