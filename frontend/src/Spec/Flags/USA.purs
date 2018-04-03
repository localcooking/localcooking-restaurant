module Spec.Flags.USA where

import Prelude (show)
import React as R
import React.DOM as RD
import React.DOM.SVG as R
import React.DOM.Props as RP


usaFlagViewBox :: String
usaFlagViewBox = "0 0 7410 3900"


usaFlag :: Array R.ReactElement
usaFlag =
    [ R.rect [RP.width "7410", RP.height "3900", RP.fill "#b22234"] []
    , R.path [RP.d "M0,450H7410m0,600H0m0,600H7410m0,600H0m0,600H7410m0,600H0", RP.stroke "#fff", RP.strokeWidth 300] []
    , R.rect [RP.width "2964", RP.height "2100", RP.y 400, RP.fill "#3c3b6e"] []
    , R.g [RP.fill "#fff"]
      [ R.g [RP._id "s18"]
        [ R.g [RP._id "s9"]
          [ R.g [RP._id "s5"]
            [ R.g [RP._id "s4"]
              [ R.path [RP._id "s", RP.d "M247,90 317.534230,307.082039 132.873218,172.917961H361.126782L176.465770,307.082039z"] []
              , use [linking "#s", RP.y 420] []
              , use [linking "#s", RP.y 840] []
              , use [linking "#s", RP.y 1260] []
              ]
            , use [linking "#s", RP.y 1680] []
            ]
          , use [linking "#s4", RP.x 247, RP.y 210] []
          ]
        , use [linking "#s9", RP.x 494] []
        ]
      , use [linking "#s18", RP.x 988] []
      , use [linking "#s9", RP.x 1976] []
      , use [linking "#s5", RP.x 2470] []
      ]
    ]
  where
    linking s = RP.unsafeMkProps "xlink:href" s

    use = RD.mkDOM (RD.IsDynamic false) "use"
