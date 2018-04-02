module MaterialUI.Markdown where

import Prelude (Unit)
import React (ReactClass, createClassStateless', ReactElement)
import React.Markdown as MD

import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.List (list)
import MaterialUI.ListItem (listItem)


markdown :: String -> ReactElement
markdown source =
  MD.markdown
  { source
  , renderers:
    { paragraph: createClassStateless' \(_ :: Unit) children ->
      [ typography
        { variant: Typography.body1
        , align: Typography.left
        }
        children
      ]
    , list: createClassStateless' \{ordered} children ->
      [ list {}
        children
      ]
    , listItem: createClassStateless' \{checked} children ->
      [ listItem {}
        children
      ]
    }
  }
