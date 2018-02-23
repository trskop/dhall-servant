{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:      Servant.Dhall
-- Description: Dhall mimetype support for Servant.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Support for using Dhall configuration language as a HTTP request\/response
-- body with mimetype @application/x-dhall@.
module Servant.Dhall
    ( Dhall
    )
  where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Semigroup ((<>))

import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy.ByteString (toStrict)
import qualified Data.Text.Buildable as Text.Builder (build)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy.Encoding as Lazy.Text (decodeUtf8', encodeUtf8)
import qualified Data.Text.Lazy.Builder as Text.Builder (toLazyText)
import qualified Text.Trifecta.Delta as Trifecta (Delta(Directed))
import qualified Dhall
    ( Interpret
    , InvalidType(InvalidType)
    , Type(Type, expected, extract)
    , auto
    )
import qualified Dhall.Core as Dhall (Expr(Annot, Note), Path, normalize)
import qualified Dhall.Context (empty)
import qualified Dhall.Parser (ParseError(ParseError), Src(Src), exprFromText)
import qualified Dhall.TypeCheck (typeWith)
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept(..), {-MimeRender(..),-} MimeUnrender(..))


data Dhall

-- | @application/x-dhall@
instance Accept Dhall where
  contentType _ = "application" // "x-dhall"

--instance Dhall.Inject a => MimeRender Dhall a where
--    mimeRender _ = encodeDhall

instance Dhall.Interpret a => MimeUnrender Dhall a where
    mimeUnrender _ = decodeDhall

decodeDhall :: Dhall.Interpret a => Lazy.ByteString -> Either String a
decodeDhall = decodeDhall' Dhall.auto

-- TODO: Traverse Dhall errors and remove terminal escape sequences.
decodeDhall' :: Dhall.Type a -> Lazy.ByteString -> Either String a
decodeDhall' Dhall.Type{..} =
    decodeUtf8 >=> parseDhallExpression >=> typeCheckAndNormalize
  where
    decodeUtf8 = first show . Lazy.Text.decodeUtf8'

    parseDhallExpression =
        first showError . (Dhall.Parser.exprFromText delta >=> failOnImport)
      where
        delta = Trifecta.Directed "(input)" 0 0 0 0

        showError (Dhall.Parser.ParseError doc) = show doc

        failOnImport expr = case traverse (const Nothing) expr of
            Nothing -> Left (Dhall.Parser.ParseError "Imports aren't allowed")
            Just expr' -> pure expr'

    typeCheckAndNormalize expr = do
        _ <- first show
            $ Dhall.TypeCheck.typeWith Dhall.Context.empty annotatedExpr

        case extract (Dhall.normalize expr) of
            Nothing -> Left (show Dhall.InvalidType)
            Just x -> pure x
      where
        annotatedExpr = case expr of
            Dhall.Note (Dhall.Parser.Src begin end bytes) _ ->
                let bytes' = bytes <> " : " <> mkSuffix expr
                in  Dhall.Note
                        (Dhall.Parser.Src begin end bytes')
                        (Dhall.Annot expr expected)

            _ -> Dhall.Annot expr expected

        mkSuffix =
            Lazy.ByteString.toStrict
            . Lazy.Text.encodeUtf8
            . Text.Builder.toLazyText
            . Text.Builder.build
