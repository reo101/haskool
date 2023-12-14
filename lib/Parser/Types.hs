module Parser.Types (
  ExtraInfo(..),
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
import Typist.Types (Type)

-- NOTE: Strings in COOL may only be up to 1024 characters
-- Consider wrapping the SConstant type in Either
-- TODO: There are more restrictions on strings which must be taken into account.
-- https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf

data ExtraInfo where
  ExtraInfo ::
    { endLine :: Int
    , typeName :: Maybe Type
    } ->
    ExtraInfo
  deriving stock (Show)

data SBinding where
  SBinding ::
    { extraInfo :: ExtraInfo
    , bidentifier :: T.Text
    , btype :: T.Text
    , bbody :: Maybe SExpr
    } ->
    SBinding
  deriving stock (Show)

data SCaseProng where
  SCaseProng ::
    { extraInfo :: ExtraInfo
    , pidenifier :: T.Text
    , ptype :: T.Text
    , pbody :: SExpr
    } ->
    SCaseProng
  deriving stock (Show)

data SProgram where
  SProgram ::
    { extraInfo :: ExtraInfo
    , pclasses :: NonEmpty SClass
    } ->
    SProgram
  deriving stock (Show)

data SClass where
  SClass ::
    { extraInfo :: ExtraInfo
    , name :: T.Text
    , parent :: Maybe T.Text
    , features :: [SFeature]
    } ->
    SClass
  deriving stock (Show)

data SFeature where
  SFeatureMember ::
    { extraInfo :: ExtraInfo
    , fbinding :: SBinding
    } ->
    SFeature
  SFeatureMethod ::
    { extraInfo :: ExtraInfo
    , fidentifier :: T.Text
    , fformals :: [SFormal]
    , ftype :: T.Text
    , fbody :: SExpr
    } ->
    SFeature
  deriving stock (Show)

data SFormal where
  SFormal ::
    { extraInfo :: ExtraInfo
    , fidentifier :: T.Text
    , ftype :: T.Text
    } ->
    SFormal
  deriving stock (Show)

data SExpr where
  SEAssignment ::
    { extraInfo :: ExtraInfo
    , aid :: T.Text
    , abody :: SExpr
    } ->
    SExpr
  SEMethodCall ::
    { extraInfo :: ExtraInfo
    , mcallee :: SExpr
    , mtype :: Maybe T.Text
    , mname :: T.Text
    , marguments :: [SExpr]
    } ->
    SExpr
  SEIfThenElse ::
    { extraInfo :: ExtraInfo
    , iif :: SExpr
    , ithen :: SExpr
    , ielse :: SExpr
    } ->
    SExpr
  SEWhile ::
    { extraInfo :: ExtraInfo
    , wif :: SExpr
    , wloop :: SExpr
    } ->
    SExpr
  SEBlock ::
    { extraInfo :: ExtraInfo
    , bexpressions :: NonEmpty SExpr
    } ->
    SExpr
  SELetIn ::
    { extraInfo :: ExtraInfo
    , lbindings :: NonEmpty SBinding
    , lbody :: SExpr
    } ->
    SExpr
  SECase ::
    { extraInfo :: ExtraInfo
    , cexpr :: SExpr
    , cprongs :: NonEmpty SCaseProng
    } ->
    SExpr
  SENew ::
    { extraInfo :: ExtraInfo
    , ntype :: T.Text
    } ->
    SExpr
  SEIsVoid ::
    { extraInfo :: ExtraInfo
    , iexpr :: SExpr
    } ->
    SExpr
  SEPlus ::
    { extraInfo :: ExtraInfo
    , pleft :: SExpr
    , pright :: SExpr
    } ->
    SExpr
  SEMinus ::
    { extraInfo :: ExtraInfo
    , mleft :: SExpr
    , mright :: SExpr
    } ->
    SExpr
  SETimes ::
    { extraInfo :: ExtraInfo
    , tleft :: SExpr
    , tright :: SExpr
    } ->
    SExpr
  SEDivide ::
    { extraInfo :: ExtraInfo
    , dleft :: SExpr
    , dright :: SExpr
    } ->
    SExpr
  SETilde ::
    { extraInfo :: ExtraInfo
    , texpr :: SExpr
    } ->
    SExpr
  SELt ::
    { extraInfo :: ExtraInfo
    , lleft :: SExpr
    , lright :: SExpr
    } ->
    SExpr
  SELte ::
    { extraInfo :: ExtraInfo
    , lleft :: SExpr
    , lright :: SExpr
    } ->
    SExpr
  SEEquals ::
    { extraInfo :: ExtraInfo
    , eleft :: SExpr
    , eright :: SExpr
    } ->
    SExpr
  SENot ::
    { extraInfo :: ExtraInfo
    , nexpr :: SExpr
    } ->
    SExpr
  SEBracketed ::
    { extraInfo :: ExtraInfo
    , bexpr :: SExpr
    } ->
    SExpr
  SEIdentifier ::
    { extraInfo :: ExtraInfo
    , iid :: T.Text
    } ->
    SExpr
  SEInteger ::
    { extraInfo :: ExtraInfo
    , iint :: Integer
    } ->
    SExpr
  SEString ::
    { extraInfo :: ExtraInfo
    , sstring :: T.Text
    } ->
    SExpr
  SEBool ::
    { extraInfo :: ExtraInfo
    , bbool :: Bool
    } ->
    SExpr
  deriving stock (Show)
