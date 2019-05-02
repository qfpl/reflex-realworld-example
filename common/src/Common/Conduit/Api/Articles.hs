{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Common.Conduit.Api.Articles where

import           Servant.API

import           Data.Text                                    (Text)
import           Servant.Auth                                 (Auth, JWT)

import           Common.Conduit.Api.Articles.Article       (Article)
import           Common.Conduit.Api.Articles.Articles      (Articles)
import           Common.Conduit.Api.Articles.Attributes    (CreateArticle)
import           Common.Conduit.Api.Articles.Comment       (Comment)
import           Common.Conduit.Api.Articles.CreateComment (CreateComment)
import           Common.Conduit.Api.Namespace              (Namespace)

type ArticlesApi token =
  (
    QueryParam "limit" Integer
  :> QueryParam "offset" Integer
  :> QueryParams "tag" Text
  :> QueryParams "author" Text
  :> QueryParams "favorited" Text
  :> Auth '[JWT] token
  :> Get '[JSON] Articles
  ) :<|> (
    Auth '[JWT] token
  :> ReqBody '[JSON] (Namespace "article" CreateArticle)
  :> PostCreated '[JSON] (Namespace "article" Article)
  ) :<|> (
    Capture "slug" Text
    :> (
      Get '[JSON] (Namespace "article" Article)
      :<|> "comments" :> (
        Get '[JSON] (Namespace "comments" [Comment])
        :<|> (
          Auth '[JWT] token
          :> ReqBody '[JSON] (Namespace "comment" CreateComment)
          :> PostCreated '[JSON] (Namespace "comment" Comment)
        ) :<|> (
          Capture "commentId" Int
          :> Auth '[JWT] token
          :>  DeleteNoContent '[JSON] NoContent)
        )
      )
    )
