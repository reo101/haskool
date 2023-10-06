{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use let" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Haskool.Test.HaskoolSpec (
  spec,
) where

import Control.Lens.Properties (isTraversal)
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Haskool (
 )
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
  shouldNotSatisfy,
  shouldSatisfy,
 )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Arbitrary (arbitrary),
  CoArbitrary,
  Function,
  Gen,
  chooseInt,
  forAll,
  oneof,
  withMaxSuccess,
 )

spec :: Spec
spec = describe "Board should be good" do
  pure ()
