{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

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
-- A DI module maps bindings.
newtype Module (all :: [*]) ins outs = Module (HList ins -> HList outs)

emptyModule :: Module all ins ins
emptyModule = Module id

inject :: Module all '[] all -> (HList all -> a) -> a
inject (Module m) f = f (m HEmpty)


bind :: a -> Module all ins (a ': ins)
bind a = Module $ \ins -> HCons a ins

install :: Module all ins x -> Module all x outs -> Module all ins outs
install (Module f) (Module g) = Module (g . f)


-- | Applies a function to the first occurrence of 'a' in 'ins'.
replace :: CanReplace0 a b ins => (a -> b) -> Module all ins (Replace0 a b ins)
replace f = Module $ \ins -> hmap f ins

-- | Applies a function to the first occurrence of a 't a' in 'ins'.
replace1 :: CanReplace1 t a b ins => (t a -> t b) -> Module all ins (Replace1 t b ins)
replace1 f = Module $ \ins -> hmap1 f ins
