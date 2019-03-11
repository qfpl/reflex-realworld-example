{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module RealWorld.Conduit.Api.Articles where

import           Data.Text                                    (Text)
import           Servant.API                                  ((:<|>), (:>),
                                                               Capture, Get,
                                                               JSON,
                                                               PostCreated,
                                                               QueryParam,
                                                               QueryParams,
                                                               ReqBody)
import           Servant.Auth                                 (Auth, JWT)

import           RealWorld.Conduit.Api.Articles.Article       (Article)
import           RealWorld.Conduit.Api.Articles.Articles      (Articles)
import           RealWorld.Conduit.Api.Articles.Attributes    (CreateArticle)
import           RealWorld.Conduit.Api.Articles.Comment       (Comment)
import           RealWorld.Conduit.Api.Articles.CreateComment (CreateComment)
import           RealWorld.Conduit.Api.Namespace              (Namespace)

type ArticlesApi =
  (
    QueryParam "limit" Integer
  :> QueryParam "offset" Integer
  :> QueryParams "tag" Text
  :> QueryParams "author" Text
  :> QueryParams "favorited" Text
  :> Auth '[JWT] Int
  :> Get '[JSON] Articles
  ) :<|> (
    Auth '[JWT] Int
  :> ReqBody '[JSON] (Namespace "article" CreateArticle)
  :> PostCreated '[JSON] (Namespace "article" Article)
  ) :<|> (
    Capture "slug" Text
    :> (
      Get '[JSON] (Namespace "article" Article)
      :<|> "comments" :> (
        Get '[JSON] (Namespace "comments" [Comment])
        :<|> (
          Auth '[JWT] Int
          :> ReqBody '[JSON] (Namespace "comment" CreateComment)
          :> PostCreated '[JSON] (Namespace "comment" Comment)
        )
      )
    )
  )
