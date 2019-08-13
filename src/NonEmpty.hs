{-# language
        DeriveAnyClass
      , DeriveFoldable
      , DeriveFunctor
      , DeriveGeneric
      , DeriveTraversable
      , DerivingStrategies
  #-}

{-| This module provides a way to lift potentially empty structures
    into one which is guaranteed to be NonEmpty by construction.
-}

module NonEmpty
  ( NonEmpty(..)

  , head
  , tail
  , toList
  , zip
  , zipWith
  , unzip
  , nonEmpty
  ) where

import Control.Comonad
import Control.Comonad.Hoist.Class
import Control.Monad.Zip
import Data.Foldable hiding (toList)
import Data.Functor.Apply
import Data.Semigroup.Foldable.Class
import GHC.Generics (Generic, Generic1)
import Prelude hiding (head, tail,zip,zipWith,unzip)
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

-- | A structure which is nonempty by construction.
--
--   Typically this will be used to construct list-like structures; e.g.
--
--   * @NonEmpty [] a@ is a lazy list containing at least one element.
--
--   * @NonEmpty (NonEmpty []) a@ is a lazy list containing at least two
--   elements.
--
--   * @NonEmpty Maybe a@ is a list that contains one or two elements.
data NonEmpty f a = NonEmpty a (f a)
  deriving stock (Functor, Foldable, Traversable)
  deriving stock (Generic, Generic1)
  deriving stock (Show, Read)
  deriving stock (Eq, Ord)
  deriving anyclass (ComonadApply)

instance Applicative f => Applicative (NonEmpty f) where
  pure x = NonEmpty x (pure x)
  (<*>) = apNonEmpty (<*>)

instance Apply f => Apply (NonEmpty f) where
  (<.>) = apNonEmpty (<.>)

apNonEmpty :: ()
  => (f (a -> b) -> f a -> f b)
  -> NonEmpty f (a -> b)
  -> NonEmpty f a
  -> NonEmpty f b
apNonEmpty ap (NonEmpty f fs) (NonEmpty x xs) = NonEmpty (f x) (ap fs xs)
{-# inline apNonEmpty #-}

instance (Applicative f, Comonad f) => Comonad (NonEmpty f) where
  extract = head
  duplicate w@(NonEmpty _ f) = NonEmpty w (fmap pure f)

instance ComonadHoist NonEmpty where
  cohoist f (NonEmpty x w) = NonEmpty x (f w)

-- i don't understand trace comonads yet, so i won't include this.
--instance (Monoid m, ComonadTraced m w) => ComonadTraced m (NonEmpty w) where

-- Is this lawful? What are the laws of ComonadTrans?
--instance ComonadTrans NonEmpty where
--  lower = tail

instance (Foldable f) => Foldable1 (NonEmpty f) where
  fold1 (NonEmpty a f) = fold' a f
  {-# inline fold1 #-}
  foldMap1 h (NonEmpty a f) = foldMap' a h f
  {-# inline foldMap1 #-}
  toNonEmpty (NonEmpty a f) = toNonEmpty' a f
  {-# inline toNonEmpty #-}

-- | Get the head of a 'NonEmpty'.
head :: NonEmpty f a -> a
head ~(NonEmpty a _) = a
{-# inline head #-}

-- | Get the 'tail' of a 'NonEmpty'.
tail :: NonEmpty f a -> f a
tail ~(NonEmpty _ f) = f
{-# inline tail #-}

-- | Convert a 'NonEmpty' into a list.
toList :: Foldable f => NonEmpty f a -> [a]
toList ~(NonEmpty a f) = a : F.toList f
{-# inline toList #-}

-- | Zip two 'NonEmpty's together.
zip :: (MonadZip f)
  => NonEmpty f a
  -> NonEmpty f b
  -> NonEmpty f (a,b)
zip = zipWith (,)
{-# inline zip #-}

-- | Zip two 'NonEmpty's together with a combining function.
zipWith :: (MonadZip f)
  => (a -> b -> c)
  -> NonEmpty f a
  -> NonEmpty f b
  -> NonEmpty f c
zipWith f ~(NonEmpty a fa) ~(NonEmpty b fb)
  = NonEmpty (f a b) (mzipWith f fa fb)
{-# inline zipWith #-}

-- | Unzip a 'NonEmpty'.
unzip :: (Functor f)
  => NonEmpty f (a,b)
  -> (NonEmpty f a, NonEmpty f b)
unzip = NE.unzip
{-# inline unzip #-}

-- | Construct a 'NonEmpty'.
nonEmpty :: a -> f a -> NonEmpty f a
nonEmpty = NonEmpty
{-# inline nonEmpty #-}

-- Internal --
toNonEmpty' :: (Foldable t) => a -> t a -> NE.NonEmpty a
toNonEmpty' a xs = a NE.:| F.toList xs
{-# inline toNonEmpty' #-}

foldMap' :: (Semigroup m, Foldable t) => a -> (a -> m) -> t a -> m
foldMap' z0 f = foldr ((<>) . f) (f z0)
{-# inline foldMap' #-}

fold' :: (Semigroup m, Foldable t) => m -> t m -> m
fold' z0 = foldMap' z0 id
{-# inline fold' #-}
