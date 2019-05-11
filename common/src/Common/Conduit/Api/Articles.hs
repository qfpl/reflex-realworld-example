{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}
module Common.Conduit.Api.Articles
  ( ArticlesApi
  , ArticleApi
  , module Article
  , module Articles
  , module Attributes
  , module Comment
  , module Namespace
  ) where

import Servant.API

import Data.Text    (Text)
import Servant.Auth (Auth, JWT)

import Common.Conduit.Api.Articles.Article       as Article (Article (Article))
import Common.Conduit.Api.Articles.Articles      as Articles (Articles (Articles))
import Common.Conduit.Api.Articles.Attributes    as Attributes (ArticleAttributes (ArticleAttributes), CreateArticle,
                                                  UpdateArticle)
import Common.Conduit.Api.Articles.Comment       as Comment (Comment (Comment))
import Common.Conduit.Api.Articles.CreateComment as Comment (CreateComment (CreateComment))
import Common.Conduit.Api.Namespace              as Namespace (Namespace (Namespace))

type ArticlesApi token =
  (
     Auth '[JWT] token
  :> QueryParam "limit" Integer
  :> QueryParam "offset" Integer
  :> QueryParams "tag" Text
  :> QueryParams "author" Text
  :> QueryParams "favorited" Text
  :> Get '[JSON] Articles
  ) :<|> (
    Auth '[JWT] token
  :> ReqBody '[JSON] (Namespace "article" CreateArticle)
  :> PostCreated '[JSON] (Namespace "article" Article)
  ) :<|> (
    "feed"
  :> Auth '[JWT] token
  :> QueryParam "limit" Integer
  :> QueryParam "offset" Integer
  :> Get '[JSON] Articles
  )
  :<|> ArticleApi token


type ArticleApi token = (
  Auth '[JWT] token
  :> Capture "slug" Text
  :> (
    Get '[JSON] (Namespace "article" Article)
    :<|> "comments" :> (
      Get '[JSON] (Namespace "comments" [Comment])
      :<|> (
        ReqBody '[JSON] (Namespace "comment" CreateComment)
        :> PostCreated '[JSON] (Namespace "comment" Comment)
      ) :<|> (
        Capture "commentId" Int
        :>  DeleteNoContent '[JSON] NoContent)
      )
    )
  )
