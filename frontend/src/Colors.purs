module Colors where

import MaterialUI.MuiThemeProvider (ColorPalette)


palette :: {primary :: ColorPalette, secondary :: ColorPalette}
palette =
  { primary:
    { light: "#5e92f3"
    , main: "#1565c0"
    , dark: "#003c8f"
    , contrastText: "#ffffff"
    }
  , secondary:
    { light: "#b6ffff"
    , main: "#81d4fa"
    , dark: "#4ba3c7"
    , contrastText: "#000000"
    }
  }
