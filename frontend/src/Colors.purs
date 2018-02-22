module Colors where

import MaterialUI.MuiThemeProvider (ColorPalette)


palette :: {primary :: ColorPalette, secondary :: ColorPalette}
palette =
  { primary:
    { light: "#ff5f52"
    , main: "#c62828"
    , dark: "#8e0000"
    , contrastText: "#ffffff"
    }
  , secondary:
    { light: "#ffe97d"
    , main: "#ffb74d"
    , dark: "#c88719"
    , contrastText: "#000000"
    }
  }
