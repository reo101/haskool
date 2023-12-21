{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Utils.Algorithms (
  dagToTree,
  subtype,
  lca,
  extendO,
  classHierarchyTree,
  classHierarchyGraph,
  allClassesWithParents,
  findFirstDuplicate,
  extractInfo,
  findInM,
) where

import Control.Arrow (Arrow (..))
import Control.Comonad.Traced (Endo (Endo, appEndo))
import Control.Lens (Field1 (_1), Field2 (_2), Field3 (_3), Lens', LensLike', Traversable1 (traverse1), over, to, toNonEmptyOf, traversed, traversed1, (&), (.~), (^.), (^..))
import Data.Generics.Labels ()
import Data.List ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE (filter, toList, zip)
import Data.List.NonEmpty.Extra ((|:))
import Data.List.NonEmpty.Extra qualified as NE (fromList, head)
import Data.Map qualified as M (empty, fromList, union, (!?))
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEM (
  fromList,
  insertWith,
  singleton,
  toList,
  (!?),
 )
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T (pack)
import Data.Tuple.Extra (both, dupe)
import Debug.Trace (trace, traceShow, traceShowId)
import Parser.Types (
  ExtraInfo (..),
  SBinding (..),
  SClass (..),
  SFeature (..),
  SFormal,
  SProgram (..),
 )
import Text.Printf (printf)
import Typist.Types (
  Class,
  Graph,
  Identifier,
  O,
  Path,
  Tree (..),
  Type, M,
 )

-- >>> dagToTree $ NEM.fromList $ NE.fromList [("1", ["3"]), ("2", ["3"]), ("3", ["4", "5"]), ("4", []), ("5", ["5"])]
-- Left ["5"]

{- | Detect cycles in a graph (has to have only one connected component)
dagToTree :: forall a. (Ord a, Show a) => Graph a -> Either (NonEmpty a) (Tree a)
-}
dagToTree :: Graph Class -> Either (NonEmpty Class) (Tree Class)
dagToTree graph = relaxNode [] start
 where
  -- start :: a
  start :: Class
  -- start = fst $ NE.head $ NEM.toList graph
  start = "Object"

  -- relaxNode :: Path a -> a -> Either (NonEmpty a) (Tree a)
  relaxNode :: Path Class -> Class -> Either (NonEmpty Class) (Tree Class)
  relaxNode path curr =
    if
        -- Found a cycle
        | curr `elem` path ->
            Left $ NE.fromList $ dropWhile (/= curr) $ reverse path
        -- Still room to go
        | not $ null edges ->
            Tree curr <$> traverse (relaxNode (curr : path)) edges
        -- Leaf, return 'singleton' tree
        | otherwise ->
            Right $ Tree curr []
   where
    -- edges :: [a]
    edges :: [Class]
    edges =
      fromMaybe
        (error $ printf "Incomplete tree (undefined node %s)" (show curr))
        (graph NEM.!? curr)

-- >>> lca (Tree 1 [Tree 2 [], Tree 3 [Tree 4 [], Tree 5 []]]) 4 5
-- 3

commonPrefix :: (Eq a) => ([a], [a]) -> [a]
commonPrefix (l1, l2) = fst <$> takeWhile (uncurry (==)) (zip l1 l2)

lca :: forall a. (Show a, Eq a) => Tree a -> a -> a -> a
lca tree x y = trace ("IVE BEEN CALLED WITH " ++ show (tree, x, y)) $ last $ traceShow (tree, x, y) $ commonPrefix $ traceShowId $ both (explore [] tree) (x, y)
 where
  -- lca tree x y = last $ commonPrefix $ both (explore [] tree) (x, y)

  explore :: Path a -> Tree a -> a -> Path a
  explore ans tree seek =
    if seek == tree ^. #node
      then reverse $ tree ^. #node : ans
      else tree ^. #neighbours >>= (\subtree -> explore ((tree ^. #node) : ans) subtree seek)

-- >>> subtype (Tree 1 [Tree 2 [], Tree 3 [Tree 4 [], Tree 5 []]]) 1 1
-- True

lesnoTree :: Tree String
lesnoTree =
  Tree
    { node = "Object"
    , neighbours =
        [ Tree{node = "Bool", neighbours = []}
        , Tree
            { node = "IO"
            , neighbours =
                [ Tree
                    { node = "Main"
                    , neighbours = []
                    }
                ]
            }
        , Tree{node = "Int", neighbours = []}
        , Tree{node = "String", neighbours = []}
        ]
    }

-- >>> subtype lesnoTree "Int" "Int"
-- True

subtype :: (Eq a, Show a) => Tree a -> a -> a -> Bool
subtype tree x y = lca tree x y == y

-- >>> SFeatureMember "" (SBinding "" (pack "a") (pack "Int") Mothing)

-- >>> T.pack "abc"

-- >>> NE.fromList [SClass "" (pack "A") (Just $ pack "Object") [SFeatureMember "" (SBinding "" (pack "a") (pack "Int") Nothing)], SClass "" (pack "B") (Just $ pack "A") [SFeatureMember "" (SBinding "" (pack "b") (pack "Int") Nothing)], SClass "" (pack "C") (Just $ pack "B") [SFeatureMember "" (SBinding "" (pack "c") (pack "Int") Nothing)]]
-- <stderr>: hPutChar: invalid argument (cannot encode character '\8216')

extendO :: NonEmpty (SClass ExtraInfo) -> NEMap Class Class -> NEMap Class O
extendO classes dg = NEM.fromList $ ((.name) *** handler) <$> NE.zip classes inheritancePaths
 where
  inheritancePaths :: NonEmpty [Class]
  inheritancePaths = reverse . goUp . (.name) <$> classes

  -- [Object, Shape, Rectangle, Square]
  -- Ractangle { x: Char }
  -- Square { x: Int, y : Char }

  -- TODO: Separate namespaces between class functions and class fields
  handler :: [Class] -> O
  handler classes' =
    let features = (M.fromList . (f <$>) . (.features) <$> NE.filter (\x -> x.name `elem` classes') classes)
     in foldr (\acc curr -> acc `M.union` curr) M.empty features

  f :: SFeature ExtraInfo -> (Identifier, Type)
  f = \case
    SFeatureMember{fbinding = SBinding{bidentifier, btype}} -> (bidentifier, btype)
    SFeatureMethod{fidentifier, ftype} -> (fidentifier, ftype)

  goUp :: Class -> [Class]
  goUp name = case dg NEM.!? name of
    Just parent -> name : goUp parent
    Nothing -> [name]

allClassesWithParents :: NonEmpty (SProgram ExtraInfo) -> Either String (NEMap Class Class)
allClassesWithParents programs =
  let classesList = programs >>= (.pclasses)
      sclassToclassWithParent (SClass{name, parent}) = (name, fromMaybe "Object" parent)
      classesWithParentsList = NE.filter (\pair -> pair /= ("Object", "Object")) (sclassToclassWithParent <$> classesList)
      -- TODO: Ugly and ruins type guarantees, find workaround
      classesWithParents = NEM.fromList $ NE.fromList classesWithParentsList
   in maybe
        (Right classesWithParents)
        (Left . printf "Class %s is defined more than once")
        (findFirstDuplicate $ (.name) <$> NE.toList classesList)

findFirstDuplicate :: (Ord a) => [a] -> Maybe a
findFirstDuplicate = go Set.empty
 where
  go :: (Ord a) => Set a -> [a] -> Maybe a
  go _ [] = Nothing
  go seen (x : xs) =
    if Set.member x seen
      then Just x
      else go (Set.insert x seen) xs

classHierarchyGraph :: NEMap Class Class -> NEMap Class (Set Class)
classHierarchyGraph classesWithParents =
  foldl
    ( \hierarchy (name, parent) ->
        applyAll
          [ -- [name] -> parent
            NEM.insertWith
              Set.union
              parent
              (Set.singleton name)
          , -- [] -> name
            NEM.insertWith
              Set.union
              name
              Set.empty
          ]
          hierarchy
    )
    (NEM.singleton "Object" Set.empty)
    (NEM.toList classesWithParents)
 where
  applyAll :: [a -> a] -> a -> a
  applyAll = appEndo . mconcat . fmap Endo

classHierarchyTree :: NEMap Class (Set Class) -> Either (NonEmpty Class) (Tree Class)
classHierarchyTree = dagToTree . (Set.toList <$>)

extractInfo :: NonEmpty (SProgram ExtraInfo) -> [((Type, Identifier), NonEmpty Type)]
extractInfo programs =
  programs
    ^.. traversed
      . #pclasses
      . traversed
      . to getClassInfo
      . traversed
 where
  getClassInfo :: SClass ExtraInfo -> [((Type, Identifier), NonEmpty Type)]
  getClassInfo sClass =
    let className =
          sClass
            ^. #name
        methodInfo =
          sClass
            ^.. #features
              . traversed
              . #_SFeatureMethod
              . to getMethodInfo
     in first (className,) <$> methodInfo

  getMethodInfo :: (a, Identifier, [SFormal ExtraInfo], Type, c) -> (Identifier, NonEmpty Type)
  getMethodInfo (_, methodName, methodFormals, methodReturnType, _) =
    let formalType =
          methodFormals
            ^.. traversed
              . #ftype
     in (methodName, formalType |: methodReturnType)

findInM :: M -> NEMap Class Class -> (Class, Identifier) -> Maybe (NonEmpty Type)
findInM m dg (c, i) =
  trace (printf "lajno %s lajno %s lajno %s" (show m) (show dg) (show (c, i))) $
  case m M.!? (c, i) of
    Just types -> Just types
    Nothing -> case dg NEM.!? c of
      Just c' -> findInM m dg (c', i)
      Nothing -> Nothing

-- (/\) ::
--   (Functor f) =>
--   -- | Lens' c a
--   ((a -> (a, a)) -> (c -> (a, c))) ->
--   -- | Lens' c b
--   ((b -> (b, b)) -> (c -> (b, c))) ->
--   -- | Lens' c (a, b)
--   (((a, b) -> f (a, b)) -> (c -> f c))
(/\) :: LensLike' ((,) a) c a -> LensLike' ((,) b) c b -> Lens' c (a, b)
(/\) lens1 lens2 f c0 =
  let (a, _) = lens1 dupe c0
      (b, _) = lens2 dupe c0
      fab = f (a, b)
   in fmap
        ( \(a, b) ->
            let (_, c1) = lens1 (,a) c0
                (_, c2) = lens2 (,b) c1
             in c2
        )
        fab

infixl 7 /\
