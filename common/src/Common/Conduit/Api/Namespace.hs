{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Common.Conduit.Api.Namespace
  ( Namespace(..)
  , unNamespace
  ) where

import           Data.Aeson                 (FromJSON (..), ToJSON (..), object,
                                             withObject, (.:), (.=))
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           GHC.TypeLits               (KnownSymbol, Symbol, symbolVal)

newtype Namespace (ns :: Symbol) a =
  Namespace a
  deriving (Show)

unNamespace :: Namespace ns a -> a
unNamespace (Namespace a) = a

symbolToText :: KnownSymbol a => Proxy a -> Text
symbolToText = Text.pack . symbolVal

instance (KnownSymbol ns, ToJSON a) => ToJSON (Namespace ns a) where
  toJSON (Namespace a) = object [symbolToText (Proxy :: Proxy ns) .= a]

instance (KnownSymbol ns, FromJSON a) => FromJSON (Namespace ns a) where
  parseJSON =
    withObject "Namespace" $ \v ->
      Namespace <$> v .: symbolToText (Proxy :: Proxy ns)
