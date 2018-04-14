module Colors where

import MaterialUI.MuiThemeProvider (ColorPalette)


palette :: {primary :: ColorPalette, secondary :: ColorPalette}
palette =
  { primary:
    { light: "#4c8c4a"
    , main: "#1b5e20"
    , dark: "#003300"
    , contrastText: "#ffffff"
    }
  , secondary:
    { light: "#f5fd67"
    , main: "#c0ca33"
    , dark: "#8c9900"
    , contrastText: "#000000"
    }
  }
