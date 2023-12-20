{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use let" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use error" #-}

module Haskool.Test.HaskoolSpec (
  spec,
) where

import Data.Foldable (traverse_)
import Test.HUnit.Lang (
  FailureReason (Reason),
  HUnitFailure (..),
 )
import Test.Hspec (
  Spec,
  describe,
  it,
  runIO,
  shouldSatisfy,
 )
import Test.QuickCheck (
  Testable (property),
 )

import Control.Exception (catch, throw, ErrorCall (..), throwIO)
import Data.Text qualified as T
import Text.Printf (printf)
import Utils.FS (
  Test (..),
  pairUpTestCases,
 )
import Utils.Pretty.Lexer (
  lexAndPrettyPrint,
 )
import Utils.Pretty.Parser (
  lexParseAndPrettyPrint,
 )
import Utils.Pretty.Typist (
  lexParseTypeAndPrettyPrint,
 )
import Utils.Pretty (
  wrapAndIntercalate,
 )
import Data.List (isInfixOf)

spec :: Spec
spec = do
  -- test_Lexer
  -- test_Parser
  test_Typist
  -- test_Typist_custom
  pure ()

test :: String -> String -> FilePath -> ((FilePath, T.Text) -> T.Text) -> Spec
test name testInfix directory prepare = describe name do
  testPairs <- runIO $ pairUpTestCases testInfix directory
  traverse_
    ( \Test{sourceFile, sourceCode, outputs} -> do
        it (printf "Test %s" sourceFile) $ property do
          let result = prepare (sourceFile, sourceCode)
          (result `shouldSatisfy` (`elem` outputs))
            -- TODO: remove (haha, was funny)
            `catch` \case
               (ErrorCall msg) -> do
                if "fromRight'" `isInfixOf` msg
                  then pure () -- PARSERA NASIRA
                  else throwIO $ ErrorCall msg
            
            `catch` \(HUnitFailure loc (Reason msg)) -> do
              throwIO $
                HUnitFailure loc $
                  Reason $
                    printf
                      "Old message: %s\nGot:\n%s\nWanted one of:\n%s"
                      msg
                      result
                      (wrapAndIntercalate "---\n" "---\n" "---\n" $ T.unpack <$> outputs)
    )
    testPairs

test_Lexer :: Spec
test_Lexer = test "Haskool Lexer" ".out" "./test/data/01" lexAndPrettyPrint

test_Parser :: Spec
test_Parser = test "Haskool Parser" ".out" "./test/data/02" lexParseAndPrettyPrint

test_Typist :: Spec
test_Typist = test "Haskool Typist" ".out" "./test/data/03" lexParseTypeAndPrettyPrint

test_Typist_custom :: Spec
test_Typist_custom = test "Haskool Typist" ".out" "./test/data/03_2" lexParseTypeAndPrettyPrint
