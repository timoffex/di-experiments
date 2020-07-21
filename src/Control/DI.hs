{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Control.DI
  (
  -- * Dependency injection
    Module
  , emptyModule
  , inject
  , bind
  , install
  , replace, replace1
  -- * Re-export of heterogeneous lists
  , module Data.HList
  ) where

import           Data.HList

-- | A dependency injection (DI) module.
--
-- A DI module is a function that maps a heterogeneous list to another
-- heterogeneous list. In other words, it takes a tuple and either
-- changes its elements or adds more elements.
--
-- The most basic module just appends a single element to the input
-- data, putting no requirements on the input:
--
-- @
--   bind myConfig :: Module all ins (MyConfig ': ins)
-- @
--
-- This corresponds to a "bind" or "provide" function in common DI
-- frameworks in languages like Java and C#. In those frameworks, one
-- normally binds to an interface, possibly qualified by a "token" (an
-- annotation / attribute). In Haskell, you can annotate a type by
-- creating a newtype wrapper:
--
-- @
--   -- An "annotation" that applies to any type.
--   newtype MyAnnotation a = MyAnnotation a
--
--   -- A user ID, which must always be an integer. In Java you might
--   -- write "@UserId Int64" instead, but that's only because creating
--   -- new types is cumbersome and syntactically inconvenient. In
--   -- Haskell, you can use 'coerce' from Data.Coerce for type-safe,
--   -- zero-cost convenience!
--   newtype UserId = UserId Int
-- @
--
-- DI modules can be chained together using the 'install' combinator in
-- infix notation:
--
-- @
--   audioModule =
--     soundPhysicsModule
--     `install` conceptOfHearingModule
--     `install` earModule
-- @
--
-- Once you have a module, you use it to 'inject' arguments into a
-- function:
--
-- @
--   -- Use PartialTypeSignatures to avoid writing out the transitive
--   -- list of dependencies for this function. The compiler will infer
--   -- the appropriate constraints on xs.
--   f :: _ => HList xs -> IO ()
--   f deps = do
--     -- Use hget or hget1 to extract dependencies. The compiler will infer
--     -- the constraints to add.
--     unMyAction $ hget deps
--     -- Pass deps directly to other injectable functions.
--     g deps
--
--   -- Given a module "yourModule" that satisfies all of f's dependencies,
--   -- inject like so:
--   inject yourModule f  :: IO ()
-- @
newtype Module (all :: [*]) ins outs = Module (HList ins -> HList outs)

-- | An empty module that does absolutely nothing.
emptyModule :: Module all ins ins
emptyModule = Module id

-- | Use a module to provide arguments to an injectable function.
inject :: Module all '[] all -> (HList all -> a) -> a
inject (Module m) f = f (m HEmpty)


-- | Creates a module that provides a value of type @a@.
--
-- The type @a@ should be concrete at the binding site. In general,
-- outputs should be as specific as possible and inputs should be as
-- generic as possible. If you bind a polymorphic value, the compiler
-- might not have enough information to figure out a concrete type for
-- it.
--
-- @
--   -- Binds a value of type @m ()@ for some monad @m@, but since
--   -- @m@ does not appear in the inputs or outputs of the expression,
--   -- a type for it cannot be determined.
--   (inject (bind (return ())) $
--      const "I don't use all bindings")    :: String
-- @
bind :: a -> Module all ins (a ': ins)
bind a = Module $ \ins -> HCons a ins


-- | Chains together two modules.
--
-- This should be used in infix notation. This is essentially @flip
-- (.)@: more fundamental modules should appear before higher level
-- modules.
install :: Module all ins x -> Module all x outs -> Module all ins outs
install (Module f) (Module g) = Module (g . f)


-- | Applies a function to the first occurrence of 'a' in 'ins'.
replace :: CanReplace0 a b ins => (a -> b) -> Module all ins (Replace0 a b ins)
replace f = Module $ \ins -> hmap f ins

-- | Applies a function to the first occurrence of a 't a' in 'ins'.
replace1 :: CanReplace1 t a b ins
         => (t a -> t b)
         -> Module all ins (Replace1 t b ins)
replace1 f = Module $ \ins -> hmap1 f ins
