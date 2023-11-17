{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Redundant <$>" #-}
module Utils.Pretty (
  wrapAndIntercalate,
) where

import Data.List (intercalate)

wrapAndIntercalate :: String -> String -> String -> [String] -> String
wrapAndIntercalate left middle right xs = left ++ intercalate middle xs ++ right
