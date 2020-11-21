{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

module Control.Lens.Extra
  ( aside1
  , aside2
  ) where

import           Control.Lens

-- | Applies a prism to the first element of a tuple of any length.
--
-- @
--   (Just 1, "a") ^? aside1 _Just        ==  Just (1, "a")
--   (Nothing, 3, 8, 9) ^? aside1 _Just   ==  Nothing
--   aside1 _Just # (1, "a")              == (Just 1, "a")
-- @
aside1 :: (Field1 s' a' s a, Field1 a' s' a s, Field1 s' s' s s) => Prism' s a -> Prism' s' a'
aside1 p = prism' (over _1 $ review p) (\s' -> do
                                          a <- s' ^? _1 . p
                                          return $ s' & _1 .~ a)

-- | Applies a prism to the second element of a tuple of any length.
--
-- See 'aside1' documentation for examples.
aside2 :: (Field2 s' a' s a, Field2 a' s' a s, Field2 s' s' s s) => Prism' s a -> Prism' s' a'
aside2 p = prism' (over _2 $ review p) (\s' -> do
                                          a <- s' ^? _2 . p
                                          return $ s' & _2 .~ a)
