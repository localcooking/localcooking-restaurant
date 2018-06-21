module Colors where

import MaterialUI.MuiThemeProvider (ColorPalette)


palette :: {primary :: ColorPalette, secondary :: ColorPalette}
palette =
  { primary:
    { light: "#ae52d4"
    , main: "#7b1fa2"
    , dark: "#4a0072"
    , contrastText: "#ffffff"
    }
  , secondary:
    { light: "#ffc1e3"
    , main: "#f48fb1"
    , dark: "#bf5f82"
    , contrastText: "#000000"
    }
  }
