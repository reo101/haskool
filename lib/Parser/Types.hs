module Parser.Types (
  ExtraInfo (..),
  SBinding (..),
  SCaseProng (..),
  SProgram (..),
  SClass (..),
  SFeature (..),
  SFormal (..),
  SExprF (..),
  SExpr,
)
where

import Control.Comonad.Cofree (Cofree (..))
import Control.Lens (Lens', lens)
import Data.Functor.Classes (Show1 (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- import Text.Show.Deriving (deriveShow2, makeLiftShowsPrec)

-- NOTE: Strings in COOL may only be up to 1024 characters
-- Consider wrapping the SConstant type in Either
-- TODO: There are more restrictions on strings which must be taken into account.
-- https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf

data ExtraInfo where
  ExtraInfo ::
    { endLine :: Int
    , typeName :: Maybe T.Text
    } ->
    ExtraInfo
  deriving stock (Generic, Show, Eq)

data SBinding e where
  SBinding ::
    { extraInfo :: e
    , bidentifier :: T.Text
    , btype :: T.Text
    , bbody :: Maybe (SExpr e)
    } ->
    SBinding e
  deriving stock (Generic, Show)

data SCaseProng e where
  SCaseProng ::
    { extraInfo :: e
    , pidenifier :: T.Text
    , ptype :: T.Text
    , pbody :: SExpr e
    } ->
    SCaseProng e
  deriving stock (Generic, Show)

data SProgram e where
  SProgram ::
    { extraInfo :: e
    , pclasses :: NonEmpty (SClass e)
    } ->
    SProgram e
  deriving stock (Generic, Show)

data SClass e where
  SClass ::
    { extraInfo :: e
    , name :: T.Text
    , parent :: Maybe T.Text
    , features :: [SFeature e]
    } ->
    SClass e
  deriving stock (Generic, Show)

data SFeature e where
  SFeatureMember ::
    { extraInfo :: e
    , fbinding :: SBinding e
    } ->
    SFeature e
  SFeatureMethod ::
    { extraInfo :: e
    , fidentifier :: T.Text
    , fformals :: [SFormal e]
    , ftype :: T.Text
    , fbody :: SExpr e
    } ->
    SFeature e
  deriving stock (Generic, Show)

data SFormal e where
  SFormal ::
    { extraInfo :: e
    , fidentifier :: T.Text
    , ftype :: T.Text
    } ->
    SFormal e
  deriving stock (Generic, Show)

type SExpr e = Cofree (SExprF e) e

data SExprF e r where
  SEAssignment ::
    { aid :: T.Text
    , abody :: r
    } ->
    SExprF e r
  SEMethodCall ::
    { mcallee :: r
    , mtype :: Maybe T.Text
    , mname :: T.Text
    , marguments :: [r]
    } ->
    SExprF e r
  SEIfThenElse ::
    { iif :: r
    , ithen :: r
    , ielse :: r
    } ->
    SExprF e r
  SEWhile ::
    { wif :: r
    , wloop :: r
    } ->
    SExprF e r
  SEBlock ::
    { bexpressions :: NonEmpty r
    } ->
    SExprF e r
  SELetIn ::
    { lbinding :: SBinding e
    , lbody :: r
    } ->
    SExprF e r
  SECase ::
    { cexpr :: r
    , cprongs :: NonEmpty (SCaseProng e)
    } ->
    SExprF e r
  SENew ::
    { ntype :: T.Text
    } ->
    SExprF e r
  SEIsVoid ::
    { iexpr :: r
    } ->
    SExprF e r
  SEPlus ::
    { pleft :: r
    , pright :: r
    } ->
    SExprF e r
  SEMinus ::
    { mleft :: r
    , mright :: r
    } ->
    SExprF e r
  SETimes ::
    { tleft :: r
    , tright :: r
    } ->
    SExprF e r
  SEDivide ::
    { dleft :: r
    , dright :: r
    } ->
    SExprF e r
  SETilde ::
    { texpr :: r
    } ->
    SExprF e r
  SELt ::
    { lleft :: r
    , lright :: r
    } ->
    SExprF e r
  SELte ::
    { lleft :: r
    , lright :: r
    } ->
    SExprF e r
  SEEquals ::
    { eleft :: r
    , eright :: r
    } ->
    SExprF e r
  SENot ::
    { nexpr :: r
    } ->
    SExprF e r
  SEBracketed ::
    { bexpr :: r
    } ->
    SExprF e r
  SEIdentifier ::
    { iid :: T.Text
    } ->
    SExprF e r
  SEInteger ::
    { iint :: Integer
    } ->
    SExprF e r
  SEString ::
    { sstring :: T.Text
    } ->
    SExprF e r
  SEBool ::
    { bbool :: Bool
    } ->
    SExprF e r
  deriving stock (Generic, Functor)

instance (Show e) => Show1 (SExprF e) where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> SExprF e a -> ShowS
  -- liftShowsPrec = $(makeLiftShowsPrec ''SExprF)
  liftShowsPrec = undefined
