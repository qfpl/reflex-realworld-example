{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications #-}
module Frontend.Head where

import Reflex.Dom.Core

import           Data.Char     (isSpace)
import           Data.Foldable (fold, traverse_)
import qualified Data.Map      as Map
import           Data.Text     (Text, pack)
import qualified Data.Text     as T

import Obelisk.Generated.Static

styleLink :: DomBuilder t m => Text -> m ()
styleLink href =
  elAttr "link" (Map.fromList [("href",href),("rel","stylesheet"),("type","text/css")]) blank

data FontType = Light | LightItalic | Regular | SemiBold | SemiBoldItalic | Bold | BoldItalic deriving (Eq, Ord, Enum, Show)

htmlHead :: (DomBuilder t m) => m ()
htmlHead = do
  el "title" $ text "Conduit"
  -- these are not the typesafe links so that the fonts load relatively to the css.
  styleLink "/static/ionicons/css/ionicons.min.css"
  elAttr "style" ("type"=:"text/css") $ traverse_ (uncurry gfontsFontFamily)
    [ ("Merriweather Sans",[Regular, Bold])
    , ("Source Sans Pro",[Light .. BoldItalic])
    , ("Source Serif Pro",[Regular, Bold])
    , ("Titillium Web",[Regular, Bold])
    ]
  styleLink (static @"main.css")

gfontsFontFamily :: DomBuilder t m => Text -> [FontType] -> m ()
gfontsFontFamily ffName = traverse_ (gfontsFontFace ffName)

-- This would be better done with Clay
gfontsFontFace :: DomBuilder t m => Text -> FontType -> m ()
gfontsFontFace familyName fontType = traverse_ text
  [ "@font-face {"
  , "  font-family: '" <> T.toLower familyName <> "';"
  , "  font-style: " <> fontStyle <> ";"
  , "  font-weight: " <> fontWeight <> ";"
  , fold
    [ "  src: url(",snaked,"/",noSpaces,"-",styleName,".ttf)"
    , ";"
    ]
  , "} "
  ]
  where
    styleName = pack $ show fontType
    fontWeight = case fontType of
      Light          -> "300"
      LightItalic    -> "300"
      Regular        -> "400"
      SemiBold       -> "600"
      SemiBoldItalic -> "600"
      Bold           -> "700"
      BoldItalic     -> "700"
    fontStyle = case fontType of
      LightItalic    -> "italic"
      SemiBoldItalic -> "italic"
      BoldItalic     -> "italic"
      _              -> "normal"

    noSpaces = T.filter (not . isSpace) familyName
    snaked   = T.replace " " "_" familyName
