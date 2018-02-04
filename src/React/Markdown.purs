module React.Markdown where

import Prelude
import React (ReactClass, ReactElement, createElement)
import Data.Record.Class (class Subrow)
import Data.Nullable (Nullable)


type MarkdownProps o =
  { source :: String
  | o }

type MarkdownPropsO renderers =
  ( renderers :: { | renderers }
  )

type SimpleRendererProps o =
  { children :: Array ReactElement
  , "data-sourcepos" :: Unit
  | o }


type RenderersO =
  ( root           :: ReactClass Unit
  , break          :: ReactClass Unit
  , paragraph      :: ReactClass Unit
  , emphasis       :: ReactClass Unit
  , strong         :: ReactClass Unit
  , thematicBreak  :: ReactClass Unit
  , blockquote     :: ReactClass Unit
  , delete         :: ReactClass Unit
  , link           :: ReactClass Unit
  , image          :: ReactClass Unit
  , linkReference  :: ReactClass Unit
  , imageReference :: ReactClass Unit
  , table          :: ReactClass (SimpleRendererProps ())
  , tableHead      :: ReactClass (SimpleRendererProps ())
  , tableBody      :: ReactClass (SimpleRendererProps ())
  , tableRow       :: ReactClass (SimpleRendererProps ())
  , tableCell      :: ReactClass (SimpleRendererProps (isHeader :: Boolean, align :: String))
  , list           :: ReactClass (SimpleRendererProps (start :: Nullable Int, ordered :: Boolean))
  , listItem       :: ReactClass (SimpleRendererProps (checked :: Nullable Boolean))
  , definition     :: ReactClass (SimpleRendererProps ())
  , heading        :: ReactClass (SimpleRendererProps (level :: Int))
  , inlineCode     :: ReactClass (SimpleRendererProps ())
  , code           :: ReactClass (SimpleRendererProps (language :: Nullable String, value :: String))
  , html           :: ReactClass (SimpleRendererProps (skipHtml :: Boolean, isBlock :: Boolean, escapeHtml :: Boolean, value :: ReactElement))
  , virtualHtml    :: ReactClass (SimpleRendererProps (tag :: String))
  )


foreign import markdownImpl :: forall props. ReactClass props

markdown :: forall renderers o
          . Subrow renderers RenderersO
         => Subrow o (MarkdownPropsO renderers)
         => MarkdownProps o -> ReactElement
markdown xs = createElement markdownImpl xs []
