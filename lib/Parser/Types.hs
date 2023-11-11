module Parser.Types (
  SBinding (..),
  SCaseProng (..),
  SProgram (..),
  SClass (..),
  SFeature (..),
  SFormal (..),
  SExpr (..),
)
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T

-- NOTE: Strings in COOL may only be up to 1024 characters
-- Consider wrapping the SConstant type in Either
-- TODO: There are more restrictions on strings which must be taken into account.
-- https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf

data SBinding where
  SBinding ::
    { bidentifier :: T.Text
    , btype :: T.Text
    , bbody :: Maybe SExpr
    } ->
    SBinding
  deriving stock (Show)

data SCaseProng where
  SCaseProng ::
    { pidenifier :: T.Text
    , ptype :: T.Text
    , pbody :: SExpr
    } ->
    SCaseProng
  deriving stock (Show)

data SProgram where
  SProgram ::
    NonEmpty SClass ->
    SProgram
  deriving stock (Show)

data SClass where
  SClass ::
    { name :: T.Text
    , parent :: Maybe T.Text
    , features :: [SFeature]
    } ->
    SClass
  deriving stock (Show)

data SFeature where
  SFeatureMember ::
    SBinding ->
    SFeature
  SFeatureMethod ::
    { fidentifier :: T.Text
    , fformals :: [SFormal]
    , ftype :: T.Text
    , fbody :: SExpr
    } ->
    SFeature
  deriving stock (Show)

data SFormal where
  SFormal ::
    { fidentifier :: T.Text
    , ftype :: T.Text
    } ->
    SFormal
  deriving stock (Show)

data SExpr where
  SEAssignment ::
    { aid :: T.Text
    , abody :: SExpr
    } ->
    SExpr
  SEMethodCall ::
    { mcallee :: SExpr
    , mtype :: Maybe T.Text
    , mname :: T.Text
    , marguments :: [SExpr]
    } ->
    SExpr
  SEFunctionCall ::
    { fcallee :: T.Text
    , farguments :: [SExpr]
    } ->
    SExpr
  SEIfThenElse ::
    { iif :: SExpr
    , ithen :: SExpr
    , ielse :: SExpr
    } ->
    SExpr
  SEWhile ::
    { wif :: SExpr
    , wloop :: SExpr
    } ->
    SExpr
  SEBlock ::
    { bexpressions :: NonEmpty SExpr
    } ->
    SExpr
  SELetIn ::
    { lbindings :: NonEmpty SBinding
    , lbody :: SExpr
    } ->
    SExpr
  SECase ::
    { cexpr :: SExpr
    , cprongs :: NonEmpty SCaseProng
    } ->
    SExpr
  SENew ::
    { ntype :: T.Text
    } ->
    SExpr
  SEIsVoid ::
    { iexpr :: SExpr
    } ->
    SExpr
  SEPlus ::
    { pleft :: SExpr
    , pright :: SExpr
    } ->
    SExpr
  SEMinus ::
    { mleft :: SExpr
    , mright :: SExpr
    } ->
    SExpr
  SETimes ::
    { tleft :: SExpr
    , tright :: SExpr
    } ->
    SExpr
  SEDivide ::
    { dleft :: SExpr
    , dright :: SExpr
    } ->
    SExpr
  SETilde ::
    { texpr :: SExpr
    } ->
    SExpr
  SELt ::
    { lleft :: SExpr
    , light :: SExpr
    } ->
    SExpr
  SELte ::
    { lleft :: SExpr
    , lright :: SExpr
    } ->
    SExpr
  SEEquals ::
    { eleft :: SExpr
    , eright :: SExpr
    } ->
    SExpr
  SENot ::
    { nexpr :: SExpr
    } ->
    SExpr
  SEBracketed ::
    { bexpr :: SExpr
    } ->
    SExpr
  SEIdentifier ::
    { iid :: T.Text
    } ->
    SExpr
  SEInteger ::
    { iint :: Integer
    } ->
    SExpr
  SEString ::
    { sstring :: T.Text
    } ->
    SExpr
  SEBool ::
    { bbool :: Bool
    } ->
    SExpr
  deriving stock (Show)
