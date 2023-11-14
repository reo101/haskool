{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Utils.FS (
  Test (..),
  listFilesInDirectory,
  pairUpTestCases,
) where

import Control.Applicative (Applicative (..))
import Control.Monad (filterM)
import Data.List.Extra (isInfixOf, isPrefixOf)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))

listFilesInDirectory :: FilePath -> IO [FilePath]
listFilesInDirectory dir = do
  contents <- listDirectory dir
  let files = filterM doesFileExist $ (dir </>) <$> contents
  files

data Test where
  Test ::
    { sourceFile :: FilePath
    , sourceCode :: T.Text
    , outputs :: [T.Text]
    } ->
    Test
  deriving stock (Show)

pairUpTestCases :: String -> FilePath -> IO [Test]
pairUpTestCases testInfix directory = do
  testFiles <- listFilesInDirectory directory
  let sourceFiles = filter (\testFile -> not $ testInfix `isInfixOf` testFile) testFiles
  traverse
    ( \sourceFile -> do
        sourceCode <- T.readFile sourceFile
        let outputFiles = filter (liftA2 (&&) (/= sourceFile) (sourceFile `isPrefixOf`)) testFiles
        outputs <- traverse T.readFile outputFiles
        pure $
          Test
            { sourceFile
            , sourceCode
            , outputs
            }
    )
    sourceFiles
