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
import Data.Foldable (sequenceA_, traverse_)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Test.Hspec (
  Spec,
  describe,
  it,
  runIO,
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

import Lexer (Lexeme, lexer)
import Text.Printf (printf)
import Utils.FS (
  Test (..),
  listFilesInDirectory,
  pairUpTestCases,
 )
import Utils.Pretty (
  lexAndPrettyPrint,
  lexParseAndPrettyPrint,
 )

spec :: Spec
spec = do
  test_Lexer
  -- test_Parser

test_Lexer :: Spec
test_Lexer = describe "Haskool Lexer" do
  testFiles <- runIO $ listFilesInDirectory "./test/data/01"
  testPairs <- runIO $ pairUpTestCases testFiles
  traverse_
    ( \Test{sourceFile, sourceCode, output} -> do
        it (printf "Test %s" sourceFile) $ property do
          lexAndPrettyPrint (sourceFile, sourceCode) `shouldBe` output
    )
    testPairs

test_Parser :: Spec
test_Parser = describe "Haskool Parser" do
  testFiles <- runIO $ listFilesInDirectory "./test/data/02"
  testPairs <- runIO $ pairUpTestCases testFiles
  traverse_
    ( \Test{sourceFile, sourceCode, output} -> do
        it (printf "Test %s" sourceFile) $ property do
          lexParseAndPrettyPrint (sourceFile, sourceCode) `shouldBe` output
    )
    testPairs
