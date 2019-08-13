module Main (main) where

import Control.Applicative (liftA2)
import Data.NonEmpty.Applicative
import qualified Data.List.NonEmpty as Base

import Hedgehog.Classes

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO Bool
main = lawsCheckMany
  [ ( "NonEmpty f"
    , [ functorLaws genNonEmpty1
      , foldableLaws genNonEmpty1
      , applicativeLaws genNonEmpty1
      , traversableLaws genNonEmpty1
      , comonadLaws genNonEmpty1
      ]
    )
  , ( "NonEmpty f a"
    , [ showLaws genNonEmpty
      , showReadLaws genNonEmpty
      , eqLaws genNonEmpty
      , ordLaws genNonEmpty
      -- , genericLaws genNonEmpty
      ]
    )
  ]

genNonEmpty :: Gen (NonEmpty Base.NonEmpty [Integer])
genNonEmpty = genNonEmpty1 (Gen.list (Range.linear 0 6) genInteger)

genInteger :: Gen Integer
genInteger = Gen.integral_ (Range.linear (-1000) 1000)

genNonEmpty1 :: Gen a -> Gen (NonEmpty Base.NonEmpty a)
genNonEmpty1 gen = do
  a <- gen
  as <- liftA2 (Base.:|) gen (Gen.list (Range.linear 0 6) gen)
  pure (NonEmpty a as)
