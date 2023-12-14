{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Typist (
  ) where

import Control.Applicative (Alternative (..))
import Control.Applicative.Combinators qualified (choice)
import Control.Arrow (Arrow (..))
import Control.Lens (Iso', iso, lens, makeLenses, view, (%~), (&), (+~), (^.))
import Control.Lens.Combinators (Lens')
import Control.Monad (foldM, join, void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Combinators.NonEmpty qualified as NE (endBy1, sepBy1)
import Data.Coerce (coerce)
import Data.Either.Extra (maybeToEither)
import Data.Foldable (traverse_)
import Data.List.NonEmpty as NE (NonEmpty (..))
import Data.List.NonEmpty qualified as NE (filter, last, toList, zip)
import Data.List.NonEmpty.Extra qualified as NE (nonEmpty)
import Data.Map qualified as M (Map, empty, fromList, insert, union, (!), (!?))
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEM (
  foldr,
  fromList,
  insertWith,
  singleton,
  toList,
  (!),
  (!?),
 )
import Data.Maybe (fromMaybe)
import Data.Maybe.HT (toMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Text qualified as T
import Data.Void (Void)
import Parser.Types (
  SBinding (..),
  SCaseProng (..),
  SClass (..),
  SExpr (..),
  SFeature (..),
  SFormal (..),
  SProgram (..),
 )
import Text.Printf (printf)
import Typist.Types (
  Class,
  Context (..),
  Identifier,
  O,
  Tree,
  Type,
  classHierarchy,
  currentClass,
  identifierTypes,
  methodTypes,
 )
import Utils.Algorithms (
  dagToTree,
  lca,
  subtype,
 )

extendO :: NonEmpty SClass -> NEMap Class Class -> NEMap Class O
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

  f :: SFeature -> (Identifier, Type)
  f = \case
    SFeatureMember{fbinding = SBinding{bidentifier, btype}} -> (bidentifier, btype)
    SFeatureMethod{fidentifier, ftype} -> (fidentifier, ftype)

  goUp :: Class -> [Class]
  goUp name = case dg NEM.!? name of
    Just parent -> name : goUp parent
    Nothing -> [name]

allClassesWithParents :: NonEmpty SProgram -> Either String (NEMap Class Class)
allClassesWithParents programs =
  let classesList = programs >>= (.pclasses)
      sclassToclassWithParent (SClass{name, parent}) = (name, fromMaybe "Object" parent)
      classesWithParentsList = sclassToclassWithParent <$> classesList
      classesWithParents = NEM.fromList classesWithParentsList
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
        NEM.insertWith
          Set.union
          parent
          (Set.singleton name)
          hierarchy
    )
    (NEM.singleton "Object" Set.empty)
    (NEM.toList classesWithParents)

classHierarchyTree :: NEMap Class (Set Class) -> Either (NonEmpty Class) (Tree Class)
classHierarchyTree = dagToTree . (Set.toList <$>)

typecheckSExpr :: Context -> SExpr -> Either String Type
typecheckSExpr context = \case
  SEIdentifier _ iid -> do
    let maybeVariableType :: Maybe Type
        maybeVariableType = (context ^. identifierTypes) M.!? iid
     in maybeToEither
          ( printf
              "Unbound variable %s"
              iid
          )
          maybeVariableType
  SEAssignment _ aid abody -> do
    let maybeVariableType :: Maybe Type
        maybeVariableType = (context ^. identifierTypes) M.!? aid
    variableType <-
      maybeToEither
        ( printf
            "Unbound variable %s"
            aid
        )
        maybeVariableType
    bodyType <- typecheckSExpr context abody
    early
      (subtype (context ^. classHierarchy) bodyType variableType)
      (printf "%s is not a subtype of %s" bodyType variableType)
    pure $ bodyType
  SEBool _ bbool -> do
    pure $ "Bool"
  SEInteger _ iint -> do
    pure $ "Int"
  SEString _ sstring -> do
    pure $ "String"
  SENew _ t -> do
    pure $ t'
   where
    t' :: Type
    t'
      | t == "SELF_TYPE" = context ^. currentClass
      | otherwise = t
  SEMethodCall _ mcallee Nothing mname marguments -> do
    calleeType <- typecheckSExpr context mcallee
    argumentTypes <- traverse (typecheckSExpr context) marguments
    -- BUG: :clueless:?
    let realCalleeType
          | calleeType == context ^. currentClass = context ^. currentClass
          | otherwise = calleeType
    methodType <-
      let maybeMethodType :: Maybe (NonEmpty Type)
          maybeMethodType = (context ^. methodTypes) M.!? (realCalleeType, mname)
       in maybeToEither
            ( printf
                "No such method '%s' for class '%s'"
                mname
                (context ^. currentClass)
            )
            maybeMethodType
    let (methodArgumentTypes, methodReturnType) = splitLast methodType
    traverse_
      ( \(argumentType, methodArgumentType) ->
          early
            (subtype (context ^. classHierarchy) argumentType methodArgumentType)
            (printf "%s is not a subtype of %s" argumentType methodArgumentType)
      )
      $ zip argumentTypes methodArgumentTypes
    let returnType
          | methodReturnType == "SELF_TYPE" = calleeType
          | otherwise = methodReturnType
    pure $ returnType
  SEMethodCall _ mcallee (Just mtype) mname marguments -> do
    calleeType <- typecheckSExpr context mcallee
    argumentTypes <- traverse (typecheckSExpr context) marguments
    early
      (subtype (context ^. classHierarchy) calleeType mtype)
      ( printf
          "%s is not a subtype of %s"
          calleeType
          mtype
      )
    methodType <-
      let maybeMethodType :: Maybe (NonEmpty Type)
          maybeMethodType = (context ^. methodTypes) M.!? (calleeType, mname)
       in maybeToEither
            ( printf
                "No such method '%s' for class '%s'"
                mname
                (context ^. currentClass)
            )
            maybeMethodType
    let (methodArgumentTypes, methodReturnType) = splitLast methodType
    traverse_
      ( \(argumentType, methodArgumentType) ->
          early
            (subtype (context ^. classHierarchy) argumentType methodArgumentType)
            (printf "%s is not a subtype of %s" argumentType methodArgumentType)
      )
      $ zip argumentTypes methodArgumentTypes
    let returnType
          | methodReturnType == "SELF_TYPE" = calleeType
          | otherwise = methodReturnType
    pure $ returnType
  SEIfThenElse _ e₁ e₂ e₃ -> do
    te₁ <- typecheckSExpr context e₁
    te₂ <- typecheckSExpr context e₂
    te₃ <- typecheckSExpr context e₃
    early
      (te₁ == "Bool")
      ( printf
          "Prost na gyz, predikata ti ne e predikat, a %s"
          te₁
      )
    pure $ lca (context ^. classHierarchy) te₂ te₃
  SEBlock _ bexpressions -> do
    expressionTypes <- traverse (typecheckSExpr context) bexpressions
    let lastExpressionType = NE.last expressionTypes
    pure $ lastExpressionType
  SELetIn line (binding :| bindings) lbody -> do
    let SBinding _ btype bidentifier bbody = binding
    let realBindingType
          | btype == "SELF_TYPE" = context ^. currentClass
          | otherwise = btype
    let letBody = case NE.nonEmpty bindings of
          Nothing -> lbody
          Just bindings' -> SELetIn line bindings' lbody
    case bbody of
      Just bindingBody -> do
        bindingBodyType <- typecheckSExpr context bindingBody
        early
          (subtype (context ^. classHierarchy) bindingBodyType realBindingType)
          ( printf
              "%s is not a subtype of %s"
              bindingBodyType
              realBindingType
          )
        letBodyType <-
          typecheckSExpr
            ( context
                & identifierTypes %~ M.insert bidentifier realBindingType
            )
            letBody
        pure $ letBodyType
      Nothing -> do
        letBodyType <-
          typecheckSExpr
            ( context
                & identifierTypes %~ M.insert bidentifier realBindingType
            )
            letBody
        pure $ letBodyType
  SECase _ cexpr cprongs -> do
    caseExprType <- typecheckSExpr context cexpr
    prongTypes <-
      traverse
        ( \(SCaseProng _ pidenifier ptype pbody) ->
            typecheckSExpr
              ( context
                  & identifierTypes %~ M.insert pidenifier ptype
              )
              pbody
        )
        cprongs
    -- TODO: Foldable1 foldl1 (from base-4.18.0)
    pure $ foldl1 (lca (context ^. classHierarchy)) $ NE.toList prongTypes
  SEWhile _ e₁ e₂ -> do
    te₁ <- typecheckSExpr context e₁
    te₂ <- typecheckSExpr context e₂
    early
      (te₁ == "Bool")
      ( printf
          "Prost na gyz, predikata ti ne e predikat, a %s"
          te₁
      )
    pure $ "Object"
  SEIsVoid _ e₁ -> do
    te₁ <- typecheckSExpr context e₁
    pure $ "Bool"
  SENot _ e₁ -> do
    te₁ <- typecheckSExpr context e₁
    early
      (te₁ == "Bool")
      ( printf
          "Prost na gyz, tva pred not-a ne e Bool, a %s"
          te₁
      )
    pure $ "Bool"
  SELt _ e₁ e₂ -> do
    te₁ <- typecheckSExpr context e₁
    te₂ <- typecheckSExpr context e₂
    early
      (te₁ == "Bool")
      ( printf
          "Prost na gyz, tva vlqvo ot < not-a ne e Int, a %s"
          te₁
      )
    early
      (te₂ == "Bool")
      ( printf
          "Prost na gyz, tva vdqsno ot < not-a ne e Int, a %s"
          te₂
      )
    pure $ "Bool"
  SELte _ e₁ e₂ -> do
    te₁ <- typecheckSExpr context e₁
    te₂ <- typecheckSExpr context e₂
    early
      (te₁ == "Bool")
      ( printf
          "Prost na gyz, tva vlqvo ot <= not-a ne e Int, a %s"
          te₁
      )
    early
      (te₂ == "Bool")
      ( printf
          "Prost na gyz, tva vdqsno ot <= not-a ne e Int, a %s"
          te₂
      )
    pure $ "Bool"
  SETilde _ e₁ -> do
    te₁ <- typecheckSExpr context e₁
    early
      (te₁ == "Int")
      ( printf
          "Prost na gyz, tva sled ~ ne e Int, a %s"
          te₁
      )
    pure $ "Int"
  SEPlus _ e₁ e₂ -> do
    te₁ <- typecheckSExpr context e₁
    te₂ <- typecheckSExpr context e₂
    early
      (te₁ == "Int")
      ( printf
          "Prost na gyz, tva vlqvo ot + not-a ne e Int, a %s"
          te₁
      )
    early
      (te₂ == "Int")
      ( printf
          "Prost na gyz, tva vdqsno ot + not-a ne e Int, a %s"
          te₂
      )
    pure $ "Bool"
  SEMinus _ e₁ e₂ -> do
    te₁ <- typecheckSExpr context e₁
    te₂ <- typecheckSExpr context e₂
    early
      (te₁ == "Int")
      ( printf
          "Prost na gyz, tva vlqvo ot - not-a ne e Int, a %s"
          te₁
      )
    early
      (te₂ == "Int")
      ( printf
          "Prost na gyz, tva vdqsno ot - not-a ne e Int, a %s"
          te₂
      )
    pure $ "Bool"
  SETimes _ e₁ e₂ -> do
    te₁ <- typecheckSExpr context e₁
    te₂ <- typecheckSExpr context e₂
    early
      (te₁ == "Int")
      ( printf
          "Prost na gyz, tva vlqvo ot * not-a ne e Int, a %s"
          te₁
      )
    early
      (te₂ == "Int")
      ( printf
          "Prost na gyz, tva vdqsno ot * not-a ne e Int, a %s"
          te₂
      )
    pure $ "Bool"
  SEDivide _ e₁ e₂ -> do
    te₁ <- typecheckSExpr context e₁
    te₂ <- typecheckSExpr context e₂
    early
      (te₁ == "Int")
      ( printf
          "Prost na gyz, tva vlqvo ot / not-a ne e Int, a %s"
          te₁
      )
    early
      (te₂ == "Int")
      ( printf
          "Prost na gyz, tva vdqsno ot / not-a ne e Int, a %s"
          te₂
      )
    pure $ "Bool"
  SEEquals _ e₁ e₂ -> do
    te₁ <- typecheckSExpr context e₁
    te₂ <- typecheckSExpr context e₂
    let primitive = (`elem` ["Int", "String", "Bool"])
    early
      (primitive te₁ && not (primitive te₂))
      ( printf
          "Prost na gyz, ne mojesh da sravnqvash primitivniq tip %s s neprimitivniq tip %s"
          te₁
          te₂
      )
    early
      (not (primitive te₁) && primitive te₂)
      ( printf
          "Prost na gyz, ne mojesh da sravnqvash neprimitivniq tip %s s primitivniq tip %s"
          te₁
          te₂
      )
    early
      (primitive te₁ && primitive te₂)
      ( printf
          "Prost na gyz, ne mojesh da sravnqvash razlichnite primitivni tipowe %s i %s"
          te₁
          te₂
      )
    pure $ "Bool"
  SEBracketed _ bexpr -> do
    typecheckSExpr context bexpr

typecheckFeature :: Context -> SFeature -> Either String Type
typecheckFeature context = \case
  SFeatureMember{fbinding = SBinding{bidentifier, btype, bbody = Just bbody}} -> do
    methodType <-
      let maybeMemberType :: Maybe Type
          maybeMemberType = (context ^. identifierTypes) M.!? bidentifier
       in maybeToEither
            ( printf
                "No such member '%s' for class '%s'"
                bidentifier
                (context ^. currentClass)
            )
            maybeMemberType
    bodyType <- typecheckSExpr (context & identifierTypes %~ M.insert "self" (context ^. currentClass)) bbody
    early
      (subtype (context ^. classHierarchy) bodyType methodType)
      ( printf
          "%s is not a subtype of %s"
          bodyType
          methodType
      )
    pure $ methodType
  SFeatureMember{fbinding = SBinding{bidentifier, btype, bbody = Nothing}} -> do
    methodType <-
      let maybeMemberType :: Maybe Type
          maybeMemberType = (context ^. identifierTypes) M.!? bidentifier
       in maybeToEither
            ( printf
                "No such member '%s' for class '%s'"
                bidentifier
                (context ^. currentClass)
            )
            maybeMemberType
    pure $ methodType
  SFeatureMethod{ftype, fidentifier, fformals, fbody} -> do
    let (argumentNames, argumentTypes) = unzip $ (\SFormal{fidentifier, ftype} -> (fidentifier, ftype)) <$> fformals
    methodType <-
      let maybeMethodType :: Maybe (NonEmpty Type)
          maybeMethodType = (context ^. methodTypes) M.!? (context ^. currentClass, fidentifier)
       in maybeToEither
            ( printf
                "No such method '%s' for class '%s'"
                fidentifier
                (context ^. currentClass)
            )
            maybeMethodType
    let (methodArgumentTypes, methodReturnType) = splitLast methodType
    traverse_
      ( \(argumentType, methodArgumentType) ->
          early
            (subtype (context ^. classHierarchy) argumentType methodArgumentType)
            (printf "%s is not a subtype of %s" argumentType methodArgumentType)
      )
      $ zip argumentTypes methodArgumentTypes
    bodyType <-
      typecheckSExpr
        ( context
            & identifierTypes %~ M.insert "self" (context ^. currentClass)
            & identifierTypes %~ (\m -> foldr (uncurry M.insert) m (zip argumentNames methodArgumentTypes))
            & identifierTypes %~ undefined -- TODO: petrakis atanasos (A HELLENIC MAN)
        )
        fbody
    let realMethodType
          | methodReturnType == "SELF_TYPE" = context ^. currentClass
          | otherwise = methodReturnType
    early
      (subtype (context ^. classHierarchy) bodyType realMethodType)
      (printf "%s is not a subtype of %s" bodyType realMethodType)
    pure $ methodReturnType

early :: Bool -> String -> Either String ()
early True msg = Left msg
early False _ = Right ()

maybeLeft :: Maybe a -> Either a ()
maybeLeft (Just a) = Left a
maybeLeft Nothing = Right ()

splitLast :: NonEmpty a -> ([a], a)
splitLast (x :| []) = ([], x)
splitLast (x :| xs) = (x : init xs, last xs)
