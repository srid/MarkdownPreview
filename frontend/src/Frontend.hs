{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T

import Obelisk.Frontend
import Obelisk.Route

import Reflex.Dom.Core

import Common.Route

import qualified Commonmark as CM

renderMarkdown :: T.Text -> Either CM.ParseError (CM.Html ())
renderMarkdown =
  CM.commonmark "markdown"

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Commonmark Markdown Preview"
  , _frontend_body = do
      el "h1" $ text "Commonmark Markdown Preview"

      markdownText :: Dynamic t T.Text <-
        fmap value $ textAreaElement $
          def
          & textAreaElementConfig_initialValue .~ "Hello *world*"
          & initialAttributes .~ ("style" =: "width:50%;height:15em;")

      result <- eitherDyn $ fmap renderMarkdown markdownText
      el "h2" $ text "Preview"
      dyn_ $ ffor result $ \case
        Left err ->
          dyn_ $ ffor err $ \_ -> text "Parse error"
        Right htmlVal ->
          prerender_ blank $ void $ elDynHtml' "div" $ T.pack . show <$> htmlVal
  }
