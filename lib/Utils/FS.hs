{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Utils.FS (
  Test (..),
  listFilesInDirectory,
  pairUpTestCases,
) where

import Control.Monad (filterM)
import Data.List.Extra (chunksOf, sort)
import Data.Text qualified as T
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Extra (readFile')

listFilesInDirectory :: FilePath -> IO [FilePath]
listFilesInDirectory dir = do
  contents <- listDirectory dir
  let fileNames = filterM doesFileExist (map (dir </>) contents)
  fileNames

data Test where
  Test ::
    { sourceFile :: FilePath
    , sourceCode :: T.Text
    , output :: T.Text
    } ->
    Test

pairUpTestCases :: [FilePath] -> IO [Test]
pairUpTestCases testFiles = do
  traverse
    ( \[sourceFile, outputFile] -> do
        sourceCode <- T.pack <$> readFile' sourceFile
        output <- T.pack <$> readFile' outputFile
        pure $
          Test
            { sourceFile
            , sourceCode
            , output
            }
    )
    (chunksOf 2 $ sort testFiles)
