{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{- | Implements heterogeneous lists.

There is a much more mature HList package somewhere out there, but I
implemented this for learning purposes and to not get too distracted.

Something I have not yet been able to do is join the 'CanReplace1' and
'Contains1' typeclasses. I believe it should be possible to define a
single typeclass indexed on @i@ and @xs@ that defines a lens for every
valid index for an @'HList' xs@ like so:

@
  class ValidIndex (i :: Nat) (xs :: [*]) where
    hix :: ( Functor f
           , a ~ AtIndex i xs
           )
        => Proxy i
        -> LensLike f (HList xs) (HList (ReplaceAt i b xs)) a b
@

I run into a problem when trying to define the recursive instance for
this class because GHC can't prove that

@
  ReplaceAt (i + 1) b (x ': xs) ~ x ': ReplaceAt i b xs
@

Normally one might resolve this by adding this as a constraint to the
instance, but @b@ is not a type variable in the class declaration. I
don't want to add @b@ or @a@ to the class declaration because then
it's no better than 'CanReplace1'. I tried using QuantifiedConstraints
for this and adding the following constraint:

@
  forall b. ReplaceAt (i + 1) b (x ': xs) ~ x ': ReplaceAt i b xs
@

But GHC doesn't allow type synonym family applications in such
constraints.
-}
module Data.HList
  ( HList (HEmpty, HCons)
  , HListTuple (asHList, asTuple)
  , singleton
  , hconcat
  , FirstTagged
  , Contains (hget), Contains1 (hget1)
  , Replace0, CanReplace0 (hmap), hlens, hlift, CanReplace0'
  , Replace1, CanReplace1 (hmap1), hlens1, hlift1, CanReplace1'
  , type (++)
  ) where


import           Control.Lens hiding (Contains)
import           Data.Proxy
import           GHC.TypeNats


-- | Heterogeneous list type.
--
-- Heterogeneous lists are isomorphic to tuples but are recursively
-- defined at the type level. The recursive definition makes it
-- possible to write functions that are polymorphic over any 'HList'
-- (whereas it's not possible to write a function that works for "any
-- tuple of any length").
data HList (xs :: [*]) where
  HEmpty :: HList '[]
  HCons  :: x -> HList xs -> HList (x ': xs)


-- | Isomorphism between an 'HList' and a tuple.
class HListTuple xs t | t -> xs, xs -> t where
  asHList :: t -> HList xs
  asTuple :: HList xs -> t

instance HListTuple '[a, b] (a, b) where
  asHList (a, b)                     = HCons a (HCons b HEmpty)
  asTuple (HCons a (HCons b HEmpty)) = (a, b)

instance HListTuple '[a, b, c] (a, b, c) where
  asHList (a, b, c) = HCons a (HCons b (HCons c HEmpty))
  asTuple (HCons a (HCons b (HCons c HEmpty))) = (a, b, c)



-- | Concatenates type-level lists.
type family as ++ bs where
  (a ': as) ++ bs = a ': (as ++ bs)
  '[]       ++ bs = bs


-- | A heterogeneous list of a single element.
singleton :: a -> HList '[a]
singleton = flip HCons HEmpty

-- | Concatenates two heterogeneous lists.
hconcat :: HList xs -> HList ys -> HList (xs ++ ys)
hconcat HEmpty ys       = ys
hconcat (HCons x xs) ys = HCons x (hconcat xs ys)


-- | Returns the inner argument of the first application of a
-- type-level function in the list of types.
--
-- For example, @FirstTagged t '[x, t a, t b] ~ a@.
--
-- This is used to help resolve ambiguous types in many situations.
type family FirstTagged (t :: k1 -> k) (xs :: [k]) where
  FirstTagged t (t x ': xs) = x
  FirstTagged t (  x ': xs) = FirstTagged t xs


-- | Class for extracting the first value of a type from an 'HList'.
--
-- Consider using 'Contains1' to avoid ambiguous types without having
-- to use proxies or excessive type annotations. To understand this,
-- look at this example:
--
-- @
--   newtype World a      = World { fromWorld :: a }
--   newtype UpdateHook w = UpdateHook { fromUpdateHook :: w -> w }
--   newtype RenderHook w = RenderHook { fromRenderHook :: w -> Picture }
--
--   -- Won't compile because of ambiguous types.
--   playGame xs = let initialWorld = fromWorld $ hget xs
--                     update       = fromUpdateHook $ hget xs
--                     render       = fromRenderHook $ hget xs
--                 in play initialWorld update render
--
--   play :: a -> (a -> a) -> (a -> Picture) -> IO ()
-- @
--
-- The compiler cannot infer whether you want a @World Int@, @World
-- ()@, @World (GameState, ViewState)@, etc. The function @playGame@
-- cannot be polymorphic over those possibilities because the type
-- argument to @World@ appears neither in its arguments (@HList xs@)
-- nor in its output (@IO ()@). You might wonder why the compiler
-- doesn't infer this type signature:
--
-- @
--   playGame :: ( Contains (World a) xs
--               , Contains (UpdateHook a) xs
--               , Contains (RenderHook a) xs
--               )
--            => HList xs -> IO ()
-- @
--
-- This signature still results in ambiguous types. Consider this
-- usage:
--
-- @
--   xs :: HList [ World (), World Integer
--               , UpdateHook (), UpdateHook Integer
--               , RenderHook (), RenderHook Integer ]
--   playGame xs
-- @
--
-- Should @playGame@ infer @World a ~ World ()@ or @World a ~ World
-- Integer@? It is clearly ambiguous. One solution is to give
-- @playGame@ a @Data.Proxy a@ argument and a type annotation:
--
-- @
--   -- Must annotate type on @playGame@ definition to tell the
--   -- compiler that the proxy defines the type of World to use.
--   -- Cannot use partial type signatures.
--   playGame :: ( Contains (World a) xs
--               , Contains (UpdateHook a) xs
--               , Contains (RenderHook a) xs
--               )
--            => Proxy a -> HList xs -> IO ()
--
--   -- Usage.
--   playGame (Proxy @()) xs
-- @
--
-- A solution without using a proxy is to add a type equality
-- constraint and to use scoped type variables to tie that type to the
-- one you're using in your function.
--
-- @
--   -- Can use a partial type signature for the rest of the
--   -- context because the compiler can infer it.
--   --
--   -- The "a ~ FirstTagged World xs" constraint allows the compiler
--   -- to infer a from xs.
--   playGame :: forall xs a.
--               (a ~ FirstTagged World xs, _)
--            => HList xs -> IO ()
--
--   -- Have to use ScopedTypeVariables to tell the compiler to use
--   -- the type defined in the context. Only one of these annotations
--   -- is necessary because the use of @play@ ties together the types.
--   playGame xs = let initialWorld = (fromWorld $ hget xs      :: a)
--                     update       = (fromUpdateHook $ hget xs :: a -> a)
--                     render       = (fromRenderHook $ hget xs :: a -> Picture)
--                 in play initialWorld update render
-- @
--
-- 'Contains1' has this type equality constraint in its context, so if
-- you use 'hget1' you don't have to write any type signatures!
class Contains a xs where
  -- | Gets the first value of type 'a' from the list.
  hget :: HList xs -> a

instance {-# OVERLAPPING #-} Contains a (a ': xs) where
  hget (HCons a _) = a

instance {-# OVERLAPPABLE #-} Contains a xs => Contains a (x ': xs) where
  hget (HCons x rest) = hget rest



-- | Class for extracting the first value of type @t a@ from @xs@.
--
-- @Contains1 t a xs@ implies @'Contains' (t a) xs@ and @a ~
-- 'FirstTagged' t xs@.  See the documentation for 'Contains' to
-- understand how 'hget1' can help you avoid ambiguous types without
-- extra work.
class ( a ~ FirstTagged t xs
      , Contains (t a) xs
      ) => Contains1 (t :: k -> *) (a :: k) (xs :: [*]) where
  -- | Extracts the first (left-most) value of type @t _@ from the
  -- list.
  hget1 :: HList xs -> t a

instance {-# OVERLAPPING #-} Contains1 t a (t a ': xs) where
  hget1 (HCons ta xs) = ta

instance {-# OVERLAPPABLE #-}
         ( Contains1 t a xs
         , a ~ FirstTagged t (x ': xs)
         ) => Contains1 t a (x ': xs) where
  hget1 (HCons _ xs) = hget1 xs


-- | Replaces the first @a@ in @xs@ by @b@.
type family Replace0 a b xs where
  Replace0 a a xs = xs
  Replace0 a b (a ': xs) = b ': xs
  Replace0 a b (x ': xs) = x ': Replace0 a b xs


-- TODO: Try to combine the CanReplace types with the Contains types.

-- | Convenience type synonym for 'CanReplace0' that gets rid of the
-- @b@.
--
-- Requires RankNTypes.
type CanReplace0' a xs = forall b. CanReplace0 a b xs

-- | Convenience type synonym for 'CanReplace1' that gets rid of the
-- @b@.
--
-- Requires RankNTypes. It's unfortunately not possible to
-- simultaneously get rid of the @a@.
type CanReplace1' t a xs = forall b. CanReplace1 t a b xs

-- | Class for viewing an @HList xs@ as a functor in one of its
-- elements.
class Contains a xs => CanReplace0 a b xs where
  hmap :: (a -> b) -> HList xs -> HList (Replace0 a b xs)

instance {-# OVERLAPPING #-} CanReplace0 a b (a ': xs) where
  hmap f (HCons b rest) = HCons (f b) rest

instance {-# OVERLAPPABLE #-}
         ( CanReplace0 a b xs
         , Replace0 a b (x ': xs) ~ (x ': Replace0 a b xs)
         ) => CanReplace0 a b (x ': xs) where
  hmap f (HCons x rest) = HCons x (hmap f rest)




type family Replace1 (t :: k1 -> k) (b :: k1) (xs :: [k]) where
  Replace1 t b (t x ': xs) = t b ': xs
  Replace1 t b (  x ': xs) = x ': Replace1 t b xs


class Contains1 t a xs => CanReplace1 (t :: k -> *) (a :: k) (b :: k) (xs :: [*]) where
  hmap1 :: (t a -> t b) -> HList xs -> HList (Replace1 t b xs)

instance {-# OVERLAPPING #-} CanReplace1 t a b (t a ': xs) where
  hmap1 f (HCons ta rest) = HCons (f ta) rest

instance {-# OVERLAPPABLE #-}
         ( CanReplace1 t a b xs
         , a ~ FirstTagged t (x ': xs)
         , Replace1 t b (x ': xs) ~ (x ': Replace1 t b xs)
         ) => CanReplace1 t a b (x ': xs) where
  hmap1 f (HCons x rest) = HCons x (hmap1 f rest)



hlens :: forall a b xs.
         CanReplace0 a b xs
      => Lens (HList xs) (HList (Replace0 a b xs)) a b
hlens = lens hget (\ls b -> hmap (\(a :: a) -> b) ls)

hlens1 :: CanReplace1 t a b xs
       => Lens (HList xs) (HList (Replace1 t b xs)) (t a) (t b)
hlens1 = lens hget1 (\ls tb -> hmap1 (const tb) ls)



hlift :: forall s t a b f xs.
         ( Functor f
         , CanReplace0 s t xs
         )
      => LensLike f s t a b
      -> LensLike f (HList xs) (HList (Replace0 s t xs)) a b
hlift l = hlens . l

hlift1 :: forall tag s t a b f xs.
          ( Functor f
          , CanReplace1 tag s t xs
          )
       => LensLike f (tag s) (tag t) a b
       -> LensLike f (HList xs) (HList (Replace1 tag t xs)) a b
hlift1 l = hlens1 . l
