module View exposing (rootView)

import Material.TopAppBar as TopAppBar
import Material.IconButton as IconButton
import Html exposing (..)
import Types exposing (..)

rootView : Model -> Html Msg
rootView model =
    case model of
        Failure reason ->
            text ("Error connecting to API - " ++ reason)

        Loading ->
            text "Loading..."

        Success school ->
            TopAppBar.regular TopAppBar.config
                [ TopAppBar.row []
                      [ TopAppBar.section [ TopAppBar.alignStart ]
                            [ IconButton.iconButton
                                  (IconButton.config
                                  |> IconButton.setAttributes
                                       [ TopAppBar.navigationIcon ]
                                  )
                                  "menu"
                            , Html.span [ TopAppBar.title ]
                                [ text school.sitename ]
                            ]
                      ]
                ]
