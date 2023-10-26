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
  Testable (property),
  chooseInt,
  forAll,
  oneof,
  withMaxSuccess,
 )

import Lexer (Token, lexer)
import Utils.FS (
  Test (..),
  listFilesInDirectory,
  pairUpTestCases,
 )
import Utils.Pretty (
  lexAndPrettyPrint,
 )
import Text.Printf (printf)

spec :: Spec
spec = describe "" do
  test_Lexer

test_Lexer :: Spec
test_Lexer =
  it "works" $ do
    testFiles <- listFilesInDirectory "./graders/tests"
    testPairs <- pairUpTestCases testFiles
    traverse_
      ( \Test{sourceFile, sourceCode, lexOutput} -> do
          it (printf "" 5) $ property do
            lexAndPrettyPrint (sourceFile, sourceCode) `shouldBe` lexOutput
      )
      testPairs
