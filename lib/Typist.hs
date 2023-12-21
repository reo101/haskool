{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Typist (
  typecheckSProgram,
  typecheckSClass,
  typecheckSFeature,
  typecheckSExpr,
) where

import Control.Comonad.Cofree (
  Cofree (..),
  _extract,
  _unwrap,
 )
import Control.Lens (
  Field1 (_1),
  Field2 (_2),
  Field3 (_3),
  Field4 (_4),
  Field5 (_5),
  traversed,
  use,
  (%=),
  (&),
  (.=),
  (.~),
  (<.~),
  (<?~),
  (?~),
  (^.),
  (^..),
 )
import Control.Monad.State (MonadState (..), MonadTrans (..), StateT)
import Data.Either.Extra (maybeToEither)
import Data.Foldable (traverse_)
import Data.Generics.Labels ()
import Data.List (nub)
import Data.List.NonEmpty as NE (NonEmpty (..))
import Data.List.NonEmpty qualified as NE (last, toList, unzip)
import Data.Map qualified as M (
  insert,
  member,
  union,
  (!?),
 )
import Data.Map.NonEmpty qualified as NEM (
  (!),
 )
import Data.Text qualified as T
import Debug.Trace (trace, traceM, traceShow, traceShowM)
import Parser.Types (
  ExtraInfo (..),
  SBinding (..),
  SCaseProng (..),
  SClass (..),
  SExpr,
  SExprF (..),
  SFeature (..),
  SFormal (..),
  SProgram (..),
 )
import Text.Printf (printf)
import Typist.Types (Context (classParentHirearchy, identifierTypes), Type)
import Utils.Algorithms (
  allClassesWithParents,
  classHierarchyGraph,
  classHierarchyTree,
  dagToTree,
  extendO,
  findFirstDuplicate,
  findInM,
  lca,
  subtype,
 )
import Utils.Pretty.Parser (prettyPrintSExpr, prettyPrintSFeature, prettyprintSClass)

typecheckSExpr :: SExpr ExtraInfo -> StateT Context (Either String) (Type, SExpr ExtraInfo)
typecheckSExpr s@(extraInfo@ExtraInfo{typeName, endLine} :< sexpr) = do
  context <- get
  let normalize t = if t == "SELF_TYPE" then context ^. #currentClass else t
  case sexpr of
    SEIdentifier iid -> do
      let maybeVariableType :: Maybe Type
          maybeVariableType = (context ^. #identifierTypes) M.!? iid
      t <-
        lift $
          maybeToEither
            ( printf
                "Unbound variable %s"
                iid
            )
            maybeVariableType
      traceM $
        printf
          "Takovam %s v kontekst %s i poluchih %s"
          (show iid)
          (show $ context ^. #identifierTypes)
          (show t)
      pure $ s & _extract . #typeName <?~ t
    SEAssignment aid abody -> do
      let maybeVariableType :: Maybe Type
          maybeVariableType = (context ^. #identifierTypes) M.!? aid
      variableType <-
        lift $
          maybeToEither
            ( printf
                "Unbound variable %s"
                aid
            )
            maybeVariableType
      (bodyType, typedBody) <- typecheckSExpr abody
      early
        (subtype (context ^. #classHierarchy) (normalize bodyType) (normalize variableType))
        (printf "%s is not a subtype of %s" bodyType variableType)
      pure $
        s
          & _unwrap . #_SEAssignment . _2 .~ typedBody
          & _extract . #typeName <?~ bodyType
    SEBool bbool -> do
      pure $ s & _extract . #typeName <?~ "Bool"
    SEInteger iint -> do
      pure $ s & _extract . #typeName <?~ "Int"
    SEString sstring -> do
      pure $ s & _extract . #typeName <?~ "String"
    SENew t -> do
      let t' :: Type
          t'
            | t == "SELF_TYPE" = context ^. #currentClass
            | otherwise = t
      -- pure $ (t', s & _extract . #typeName ?~ t)
      pure $ s & _extract . #typeName <?~ t
    SEMethodCall mcallee Nothing mname marguments -> do
      (calleeType, typedCallee) <- typecheckSExpr mcallee
      (argumentTypes, typedArguments) <- unzip <$> traverse typecheckSExpr marguments
      methodType <-
        let maybeMethodType :: Maybe (NonEmpty Type)
            maybeMethodType = findInM (context ^. #methodTypes) (context ^. #classParentHirearchy) (normalize calleeType, mname)
         in lift $
              maybeToEither
                ( printf
                    "No such method '%s' for class '%s' when typechecking SEMethodCall"
                    mname
                    (context ^. #currentClass)
                )
                maybeMethodType
      let (methodArgumentTypes, methodReturnType) = splitLast methodType
      traverse_
        ( \(argumentType, methodArgumentType) ->
            early
              (subtype (context ^. #classHierarchy) (normalize argumentType) (normalize methodArgumentType))
              (printf "%s is not a subtype of %s" argumentType methodArgumentType)
        )
        $ zip argumentTypes methodArgumentTypes
      let returnType
            | methodReturnType == "SELF_TYPE" = calleeType
            | otherwise = methodReturnType
      pure $
        s
          & _unwrap . #_SEMethodCall . _1 .~ typedCallee
          & _unwrap . #_SEMethodCall . _4 .~ typedArguments
          & _extract . #typeName <?~ returnType
    SEMethodCall mcallee (Just mtype) mname marguments -> do
      (calleeType, typedCallee) <- typecheckSExpr mcallee
      (argumentTypes, typedArguments) <- unzip <$> traverse typecheckSExpr marguments
      early
        (subtype (context ^. #classHierarchy) (normalize calleeType) (normalize mtype))
        ( printf
            "%s is not a subtype of %s"
            calleeType
            mtype
        )
      methodType <-
        let maybeMethodType :: Maybe (NonEmpty Type)
            maybeMethodType = findInM (context ^. #methodTypes) (context ^. #classParentHirearchy) (normalize mtype, mname)
         in lift $
              maybeToEither
                ( printf
                    "No such method '%s' for class '%s' when typechecking SEMethodCall with dispatch"
                    mname
                    calleeType
                )
                maybeMethodType
      let (methodArgumentTypes, methodReturnType) = splitLast methodType
      traverse_
        ( \(argumentType, methodArgumentType) ->
            early
              (subtype (context ^. #classHierarchy) (normalize argumentType) (normalize methodArgumentType))
              (printf "%s is not a subtype of %s" argumentType methodArgumentType)
        )
        $ zip argumentTypes methodArgumentTypes
      let returnType
            | methodReturnType == "SELF_TYPE" = calleeType
            | otherwise = methodReturnType
      pure $
        s
          & _unwrap . #_SEMethodCall . _1 .~ typedCallee
          & _unwrap . #_SEMethodCall . _4 .~ typedArguments
          & _extract . #typeName <?~ returnType
    SEIfThenElse e1 e2 e3 -> do
      (e1t, te1) <- typecheckSExpr e1
      (e2t, te2) <- typecheckSExpr e2
      (e3t, te3) <- typecheckSExpr e3
      early
        (e1t == "Bool")
        ( printf
            "Prost na gyz, predikata ti ne e predikat, a %s"
            e1t
        )
      let commonType = lca (context ^. #classHierarchy) (normalize e2t) (normalize e3t)
      pure $
        s
          & _unwrap . #_SEIfThenElse . _1 .~ te1
          & _unwrap . #_SEIfThenElse . _2 .~ te2
          & _unwrap . #_SEIfThenElse . _3 .~ te3
          & _extract . #typeName <?~ commonType
    SEBlock bexpressions -> do
      (expressionTypes, typedExpressions) <- NE.unzip <$> traverse typecheckSExpr bexpressions
      let lastExpressionType = NE.last expressionTypes
      pure $
        s
          & _unwrap . #_SEBlock .~ typedExpressions
          & _extract . #typeName <?~ lastExpressionType
    SELetIn (SBinding ei bidentifier btype bbody) lbody -> do
      let realBindingType = btype
      case bbody of
        Just bindingBody -> do
          (bindingBodyType, typedBindingBody) <- typecheckSExpr bindingBody
          early
            (subtype (context ^. #classHierarchy) (normalize bindingBodyType) (normalize realBindingType))
            ( printf
                "%s is not a subtype of %s"
                bindingBodyType
                realBindingType
            )
          (letBodyType, typedLetBody) <- do
            oldIdentifierTypes <- use #identifierTypes
            #identifierTypes %= M.insert bidentifier realBindingType
            t <- typecheckSExpr lbody
            #identifierTypes .= oldIdentifierTypes
            pure t
          pure $
            s
              & _unwrap . #_SELetIn . _1 . #bbody ?~ typedBindingBody
              & _unwrap . #_SELetIn . _1 . #extraInfo . #typeName ?~ bindingBodyType
              & _unwrap . #_SELetIn . _2 .~ typedLetBody
              & _extract . #typeName <?~ letBodyType
        Nothing -> do
          (letBodyType, typedLetBody) <- do
            oldContext <- get
            oldIdentifierTypes <- use #identifierTypes
            #identifierTypes %= M.insert bidentifier realBindingType
            t <- typecheckSExpr lbody
            #identifierTypes .= oldIdentifierTypes
            pure t
          pure $
            s
              -- & _unwrap . #_SELetIn . _1 . #bbody .~ Nothing
              & _unwrap . #_SELetIn . _2 .~ typedLetBody
              & _extract . #typeName <?~ letBodyType
    SECase cexpr cprongs -> do
      (caseExprType, typedCaseExpr) <- typecheckSExpr cexpr
      (prongBodyTypes, typedProngBodies) <-
        NE.unzip
          <$> traverse
            ( \scp@(SCaseProng _ pidenifier ptype pbody) -> do
                oldContext <- get
                oldIdentifierTypes <- use #identifierTypes
                #identifierTypes %= M.insert pidenifier ptype
                (bodyType, typedBody) <- typecheckSExpr pbody
                #identifierTypes .= oldIdentifierTypes
                pure $
                  ( bodyType
                  , scp
                      & #pbody .~ typedBody
                  )
            )
            cprongs

      -- TODO: Foldable1 foldl1 (from base-4.18.0)
      let commonType = foldl1 (lca (context ^. #classHierarchy)) $ normalize <$> NE.toList prongBodyTypes
      pure $
        s
          & _unwrap . #_SECase . _2 .~ typedProngBodies
          & _extract . #typeName <?~ commonType
    SEWhile e1 e2 -> do
      (e1t, te1) <- typecheckSExpr e1
      (e2t, te2) <- typecheckSExpr e2
      early
        (e1t == "Bool")
        ( printf
            "Prost na gyz, predikata ti ne e predikat, a %s"
            e1t
        )
      pure $
        s
          & _unwrap . #_SEWhile . _1 .~ te1
          & _unwrap . #_SEWhile . _2 .~ te2
          & _extract . #typeName <?~ "Object"
    SEIsVoid e1 -> do
      (e1t, te1) <- typecheckSExpr e1
      pure $
        s
          & _unwrap . #_SEIsVoid .~ te1
          & _extract . #typeName <?~ "Bool"
    SENot e1 -> do
      (e1t, te1) <- typecheckSExpr e1
      early
        (e1t == "Bool")
        ( printf
            "Prost na gyz, tva pred not-a ne e Bool, a %s"
            e1t
        )
      pure $
        s
          & _unwrap . #_SENot .~ te1
          & _extract . #typeName <?~ "Bool"
    SELt e1 e2 -> do
      (e1t, te1) <- typecheckSExpr e1
      (e2t, te2) <- typecheckSExpr e2
      early
        (e1t == "Bool")
        ( printf
            "Prost na gyz, tva vlqvo ot < not-a ne e Int, a %s"
            e1t
        )
      early
        (e2t == "Bool")
        ( printf
            "Prost na gyz, tva vdqsno ot < not-a ne e Int, a %s"
            e2t
        )
      pure $
        s
          & _unwrap . #_SELt . _1 .~ te1
          & _unwrap . #_SELt . _2 .~ te2
          & _extract . #typeName <?~ "Bool"
    SELte e1 e2 -> do
      (e1t, te1) <- typecheckSExpr e1
      (e2t, te2) <- typecheckSExpr e2
      early
        (e1t == "Bool")
        ( printf
            "Prost na gyz, tva vlqvo ot <= not-a ne e Int, a %s"
            e1t
        )
      early
        (e2t == "Bool")
        ( printf
            "Prost na gyz, tva vdqsno ot <= not-a ne e Int, a %s"
            e2t
        )
      pure $
        s
          & _unwrap . #_SELte . _1 .~ te1
          & _unwrap . #_SELte . _2 .~ te2
          & _extract . #typeName <?~ "Bool"
    SETilde e1 -> do
      (e1t, te1) <- typecheckSExpr e1
      early
        (e1t == "Int")
        ( printf
            "Prost na gyz, tva sled ~ ne e Int, a %s"
            e1t
        )
      pure $
        s
          & _unwrap . #_SETilde .~ te1
          & _extract . #typeName <?~ "Int"
    SEPlus e1 e2 -> do
      (e1t, te1) <- typecheckSExpr e1
      (e2t, te2) <- typecheckSExpr e2
      early
        (e1t == "Int")
        ( printf
            "Prost na gyz, tva vlqvo ot + not-a ne e Int, a %s"
            e1t
        )
      early
        (e2t == "Int")
        ( printf
            "Prost na gyz, tva vdqsno ot + not-a ne e Int, a %s"
            e2t
        )
      pure $
        s
          & _unwrap . #_SEPlus . _1 .~ te1
          & _unwrap . #_SEPlus . _2 .~ te2
          & _extract . #typeName <?~ "Int"
    SEMinus e1 e2 -> do
      (e1t, te1) <- typecheckSExpr e1
      (e2t, te2) <- typecheckSExpr e2
      early
        (e1t == "Int")
        ( printf
            "Prost na gyz, tva vlqvo ot - not-a ne e Int, a %s"
            e1t
        )
      early
        (e2t == "Int")
        ( printf
            "Prost na gyz, tva vdqsno ot - not-a ne e Int, a %s"
            e2t
        )
      pure $
        s
          & _unwrap . #_SEMinus . _1 .~ te1
          & _unwrap . #_SEMinus . _2 .~ te2
          & _extract . #typeName <?~ "Int"
    SETimes e1 e2 -> do
      (e1t, te1) <- typecheckSExpr e1
      (e2t, te2) <- typecheckSExpr e2
      early
        (e1t == "Int")
        ( printf
            "Prost na gyz, tva vlqvo ot * not-a ne e Int, a %s"
            e1t
        )
      early
        (e2t == "Int")
        ( printf
            "Prost na gyz, tva vdqsno ot * not-a ne e Int, a %s"
            e2t
        )
      pure $
        s
          & _unwrap . #_SETimes . _1 .~ te1
          & _unwrap . #_SETimes . _2 .~ te2
          & _extract . #typeName <?~ "Int"
    SEDivide e1 e2 -> do
      (e1t, te1) <- typecheckSExpr e1
      (e2t, te2) <- typecheckSExpr e2
      early
        (e1t == "Int")
        ( printf
            "Prost na gyz, tva vlqvo ot / not-a ne e Int, a %s"
            e1t
        )
      early
        (e2t == "Int")
        ( printf
            "Prost na gyz, tva vdqsno ot / not-a ne e Int, a %s"
            e2t
        )
      pure $
        s
          & _unwrap . #_SEDivide . _1 .~ te1
          & _unwrap . #_SEDivide . _2 .~ te2
          & _extract . #typeName <?~ "Int"
    SEEquals e1 e2 -> do
      (e1t, te1) <- typecheckSExpr e1
      (e2t, te2) <- typecheckSExpr e2
      let primitive = (`elem` ["Int", "String", "Bool"])
      traceShowM $ primitive e1t
      traceShowM $ primitive e2t
      early
        (not (primitive e1t && not (primitive e2t)))
        ( printf
            "Prost na gyz, ne mojesh da sravnqvash primitivniq tip %s s neprimitivniq tip %s"
            e1t
            e2t
        )
      early
        (not (not (primitive e1t) && primitive e2t))
        ( printf
            "Prost na gyz, ne mojesh da sravnqvash neprimitivniq tip %s s primitivniq tip %s"
            e1t
            e2t
        )
      early
        (not ((primitive e1t && primitive e2t) && e1t /= e2t))
        ( printf
            "Prost na gyz, ne mojesh da sravnqvash razlichnite primitivni tipowe %s i %s"
            e1t
            e2t
        )
      pure $
        s
          & _unwrap . #_SEEquals . _1 .~ te1
          & _unwrap . #_SEEquals . _2 .~ te2
          & _extract . #typeName <?~ "Bool"
    SEBracketed bexpr -> do
      (exprType, typedExpr) <- typecheckSExpr bexpr
      pure $
        s
          & _unwrap . #_SEBracketed .~ typedExpr
          & _extract . #typeName <?~ exprType

typecheckSFeature :: SFeature ExtraInfo -> StateT Context (Either String) (Type, SFeature ExtraInfo)
typecheckSFeature sfeature = do
  context <- get
  let normalize t = if t == "SELF_TYPE" then context ^. #currentClass else t
  case sfeature of
    SFeatureMember{fbinding = SBinding{bidentifier, btype, bbody = Just bbody}} -> do
      -- methodType <-
      --  let maybeMemberType :: Maybe Type
      --      maybeMemberType = (context ^. #identifierTypes) M.!? bidentifier
      --   in lift $
      --        maybeToEither
      --          ( printf
      --              "No such member '%s' for class '%s'"
      --              bidentifier
      --              (context ^. #currentClass)
      --          )
      --          maybeMemberType

      let parent = (NEM.!) (context ^. #classParentHirearchy) (context ^. #currentClass)

      let check = extendO
                  ((.pclasses) =<< (context ^. #programs))
                  (context ^. #classParentHirearchy)
                  NEM.! parent

      early
        (not $ bidentifier `M.member` check)
        (printf "Attribute %s is an attribute of an inheriting class" bidentifier)
      (bodyType, typedBody) <- do
        oldContext <- get
        oldIdentifierTypes <- use #identifierTypes
        #identifierTypes %= M.insert "self" (context ^. #currentClass)
        t <- typecheckSExpr bbody
        #identifierTypes .= oldIdentifierTypes
        pure t
      early
        (subtype (context ^. #classHierarchy) (normalize bodyType) (normalize btype))
        ( printf
            "%s is not a subtype of %s"
            bodyType
            btype
        )
      pure $
        sfeature
          & #_SFeatureMember . _2 . #bbody ?~ typedBody
          -- & #_SFeatureMember . _2 . #btype .~ bodyType
          & #extraInfo . #typeName <?~ btype
    SFeatureMember{fbinding = SBinding{bidentifier, btype, bbody = Nothing}} -> do
      -- methodType <-
      --  let maybeMemberType :: Maybe Type
      --      maybeMemberType = (context ^. #identifierTypes) M.!? bidentifier
      --   in lift $
      --        maybeToEither
      --          ( printf
      --              "No such member '%s' for class '%s'"
      --              bidentifier
      --              (context ^. #currentClass)
      --          )
      --          maybeMemberType
      let parent = (NEM.!) (context ^. #classParentHirearchy) (context ^. #currentClass)

      let check = extendO
                  ((.pclasses) =<< (context ^. #programs))
                  (context ^. #classParentHirearchy)
                  NEM.! parent

      early
        (not $ bidentifier `M.member` check)
        (printf "Attribute %s is an attribute of an inheriting class" bidentifier)
      early
        (not $ bidentifier `M.member` (context ^. #identifierTypes))
        (printf "Attribute %s is an attribute of an inheriting class" bidentifier)
      pure $
        sfeature
          & #extraInfo . #typeName <?~ btype
    SFeatureMethod{ftype, fidentifier, fformals, fbody} -> do
      -- early
      --  (length fformals /= length (nub (fformals ^.. traversed . #fidentifier)))
      --  (printf "Formal names for function %s should be unique" fidentifier)
      let (argumentNames, argumentTypes) = unzip $ (\SFormal{fidentifier, ftype} -> (fidentifier, ftype)) <$> fformals
      methodType <-
        let maybeMethodType :: Maybe (NonEmpty Type)
            maybeMethodType = (context ^. #methodTypes) M.!? (context ^. #currentClass, fidentifier)
         in lift $
              maybeToEither
                ( printf
                    "No such method '%s' for class '%s' when typechecking SFeatureMethod"
                    fidentifier
                    (context ^. #currentClass)
                )
                maybeMethodType
      let (methodArgumentTypes, methodReturnType) = splitLast methodType
      traverse_
        ( \(argumentType, methodArgumentType) ->
            early
              (subtype (context ^. #classHierarchy) (normalize argumentType) (normalize methodArgumentType))
              (printf "%s is not a subtype of %s" argumentType methodArgumentType)
        )
        $ zip argumentTypes methodArgumentTypes
      (bodyType, typedBody) <- do
        oldContext <- get
        #identifierTypes %= M.insert "self" "SELF_TYPE" -- (context ^. #currentClass)
        #identifierTypes %= (\o -> foldr (uncurry M.insert) o (zip argumentNames methodArgumentTypes))
        -- traceShow "EXTENDING O" get
        -- traceShow (extendO ((.pclasses) =<< (context ^. #programs)) (context ^. #classParentHirearchy) NEM.! (context ^. #currentClass)) get

        #identifierTypes
          %= ( \o ->
                o
                  `M.union` ( extendO
                                ((.pclasses) =<< (context ^. #programs))
                                (context ^. #classParentHirearchy)
                                NEM.! (context ^. #currentClass)
                            )
             )
        oldIdentifierTypes <- use #identifierTypes
        (bodyType, typedBody) <- typecheckSExpr fbody
        #identifierTypes .= oldIdentifierTypes
        pure (bodyType, typedBody)
      let realMethodType
            | methodReturnType == "SELF_TYPE" = context ^. #currentClass
            | otherwise = methodReturnType
      early
        (subtype (context ^. #classHierarchy) (normalize bodyType) (normalize realMethodType))
        (printf "%s is not a subtype of %s" bodyType realMethodType)
      -- traceShowExpr $ fbody
      -- traceShowExpr $ typedBody
      pure $
        sfeature
          & #_SFeatureMethod . _5 .~ typedBody
          & #_SFeatureMethod . _4 <.~ methodReturnType

hasParent :: Maybe T.Text -> Bool
hasParent mb = case mb of
  Just cidentifier -> cidentifier == T.pack "SELF_TYPE"
  Nothing -> False

typecheckSClass :: SClass ExtraInfo -> StateT Context (Either String) (SClass ExtraInfo)
typecheckSClass sclass@SClass{parent} = do
  early (not $ hasParent parent) (printf "No class can inherit SELF_TYPE")
  context <- get
  (featureTypes, typedFeatures) <- NE.unzip <$> traverse typecheckSFeature (sclass ^. #features)
  pure $
    sclass
      & #features .~ typedFeatures

typecheckSProgram :: SProgram ExtraInfo -> StateT Context (Either String) (SProgram ExtraInfo)
typecheckSProgram sprogram@(SProgram{pclasses}) = do
  -- TODO: Data.List.NonEmpty doesn't have an "elem" or "member" method ???
  early (not $ "Main" `notElem` NE.toList ((.name) <$> pclasses)) (printf "Class Main not defined")
  context <- get
  typedClasses <-
    traverse
      ( \pclass -> do
          oldContext <- get
          #currentClass .= pclass ^. #name
          typedClass <- typecheckSClass pclass

          oldIdentifierTypes <- use #identifierTypes
          #identifierTypes .= oldIdentifierTypes

          pure typedClass
      )
      (sprogram ^. #pclasses)
  -- traceShowClasses $ sprogram ^. #pclasses
  -- traceShowClasses $ typedClasses
  pure $
    sprogram
      & #pclasses .~ typedClasses

traceShowFeatures :: (Applicative f) => SFeature ExtraInfo -> f ()
traceShowFeatures = traverse_ traceShowM . prettyPrintSFeature

traceShowClasses :: (Foldable t, Applicative f) => t (SClass ExtraInfo) -> f ()
traceShowClasses = traverse_ (traceShowM . prettyprintSClass "kek")

traceShowExpr :: (Applicative f) => SExpr ExtraInfo -> f ()
traceShowExpr = traceShowM . prettyPrintSExpr

early :: Bool -> String -> StateT Context (Either String) ()
early False msg = #errors %= (T.pack msg :)
early True _ = lift $ Right ()

maybeLeft :: Maybe T.Text -> StateT Context (Either a) ()
maybeLeft (Just msg) = #errors %= (msg :)
maybeLeft Nothing = lift $ Right ()

splitLast :: NonEmpty a -> ([a], a)
splitLast (x :| []) = ([], x)
splitLast (x :| xs) = (x : init xs, last xs)
