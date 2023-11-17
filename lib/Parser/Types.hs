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
    { endLine :: Int
    , bidentifier :: T.Text
    , btype :: T.Text
    , bbody :: Maybe SExpr
    } ->
    SBinding
  deriving stock (Show)

data SCaseProng where
  SCaseProng ::
    { endLine :: Int
    , pidenifier :: T.Text
    , ptype :: T.Text
    , pbody :: SExpr
    } ->
    SCaseProng
  deriving stock (Show)

data SProgram where
  SProgram ::
    { endLine :: Int
    , pclasses :: NonEmpty SClass
    } ->
    SProgram
  deriving stock (Show)

data SClass where
  SClass ::
    { endLine :: Int
    , name :: T.Text
    , parent :: Maybe T.Text
    , features :: [SFeature]
    } ->
    SClass
  deriving stock (Show)

data SFeature where
  SFeatureMember ::
    { endLine :: Int
    , fbinding :: SBinding
    } ->
    SFeature
  SFeatureMethod ::
    { endLine :: Int
    , fidentifier :: T.Text
    , fformals :: [SFormal]
    , ftype :: T.Text
    , fbody :: SExpr
    } ->
    SFeature
  deriving stock (Show)

data SFormal where
  SFormal ::
    { endLine :: Int
    , fidentifier :: T.Text
    , ftype :: T.Text
    } ->
    SFormal
  deriving stock (Show)

data SExpr where
  SEAssignment ::
    { endLine :: Int
    , aid :: T.Text
    , abody :: SExpr
    } ->
    SExpr
  SEMethodCall ::
    { endLine :: Int
    , mcallee :: SExpr
    , mtype :: Maybe T.Text
    , mname :: T.Text
    , marguments :: [SExpr]
    } ->
    SExpr
  SEIfThenElse ::
    { endLine :: Int
    , iif :: SExpr
    , ithen :: SExpr
    , ielse :: SExpr
    } ->
    SExpr
  SEWhile ::
    { endLine :: Int
    , wif :: SExpr
    , wloop :: SExpr
    } ->
    SExpr
  SEBlock ::
    { endLine :: Int
    , bexpressions :: NonEmpty SExpr
    } ->
    SExpr
  SELetIn ::
    { endLine :: Int
    , lbindings :: NonEmpty SBinding
    , lbody :: SExpr
    } ->
    SExpr
  SECase ::
    { endLine :: Int
    , cexpr :: SExpr
    , cprongs :: NonEmpty SCaseProng
    } ->
    SExpr
  SENew ::
    { endLine :: Int
    , ntype :: T.Text
    } ->
    SExpr
  SEIsVoid ::
    { endLine :: Int
    , iexpr :: SExpr
    } ->
    SExpr
  SEPlus ::
    { endLine :: Int
    , pleft :: SExpr
    , pright :: SExpr
    } ->
    SExpr
  SEMinus ::
    { endLine :: Int
    , mleft :: SExpr
    , mright :: SExpr
    } ->
    SExpr
  SETimes ::
    { endLine :: Int
    , tleft :: SExpr
    , tright :: SExpr
    } ->
    SExpr
  SEDivide ::
    { endLine :: Int
    , dleft :: SExpr
    , dright :: SExpr
    } ->
    SExpr
  SETilde ::
    { endLine :: Int
    , texpr :: SExpr
    } ->
    SExpr
  SELt ::
    { endLine :: Int
    , lleft :: SExpr
    , lright :: SExpr
    } ->
    SExpr
  SELte ::
    { endLine :: Int
    , lleft :: SExpr
    , lright :: SExpr
    } ->
    SExpr
  SEEquals ::
    { endLine :: Int
    , eleft :: SExpr
    , eright :: SExpr
    } ->
    SExpr
  SENot ::
    { endLine :: Int
    , nexpr :: SExpr
    } ->
    SExpr
  SEBracketed ::
    { endLine :: Int
    , bexpr :: SExpr
    } ->
    SExpr
  SEIdentifier ::
    { endLine :: Int
    , iid :: T.Text
    } ->
    SExpr
  SEInteger ::
    { endLine :: Int
    , iint :: Integer
    } ->
    SExpr
  SEString ::
    { endLine :: Int
    , sstring :: T.Text
    } ->
    SExpr
  SEBool ::
    { endLine :: Int
    , bbool :: Bool
    } ->
    SExpr
  deriving stock (Show)
