module MaterialUI.Markdown where

import Prelude (Unit)
import React (ReactClass, createClassStateless', ReactElement)
import React.Markdown as MD

import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography


markdown :: String -> ReactElement
markdown source =
  MD.markdown
  { source
  , renderers:
    { paragraph: createClassStateless' \(_ :: Unit) children ->
      [ typography
        { variant: Typography.body1
        , align: Typography.justify
        }
        children
      ]
    }
  }
