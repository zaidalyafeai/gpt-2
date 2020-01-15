{-# LANGUAGE ConstraintKinds #-}
module Parse where

import           Control.Applicative
import           Control.Monad.Fail as MF
import           Data.Char
import           Data.Constraint
import           Data.Foldable
import           Data.List
import           Data.Ord
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.Trifecta as Trifecta

type Parser m = (CharParsing m, Monad m)

parse :: Trifecta.Parser a -> Text -> a
parse p txt = case Trifecta.parseString p mempty (T.unpack txt) of
  Trifecta.Success a -> a
  Trifecta.Failure e -> error (show (Trifecta._errDoc e))

parseM :: (MonadFail m) => Trifecta.Parser a -> Text -> m a
parseM p txt = case Trifecta.parseString p mempty (T.unpack txt) of
  Trifecta.Success a -> pure a
  Trifecta.Failure e -> MF.fail (show (Trifecta._errDoc e))

infixr 6 ##
(##) :: (Applicative f, Monoid a) => f a -> f a -> f a
(##) a b = liftA2 (<>) a b
