module View exposing (rootView)

import Material.TopAppBar as TopAppBar
import Material.IconButton as IconButton
import Html exposing (..)
import Html.Attributes exposing (style)
import Types exposing (..)

rootView : Model -> Html Msg
rootView model =
    case model of
        Failure reason ->
            text ("Error connecting to API - " ++ reason)

        Loading ->
            text "Loading..."

        Success siteInfo ->
            TopAppBar.regular (TopAppBar.config |> TopAppBar.setDense True)
            [ TopAppBar.row []
                [ TopAppBar.section
                    [ TopAppBar.alignStart ]
                    [ IconButton.iconButton
                        (IconButton.config
                            |> IconButton.setAttributes [ TopAppBar.navigationIcon ]
                        )
                        "menu"
                    , Html.span [ TopAppBar.title ] [ text siteInfo.sitename ]
                    ]
                , TopAppBar.section
                    [ TopAppBar.alignEnd ]
                    [  Html.span [ TopAppBar.title ] [ text siteInfo.fullname ]
                    ,IconButton.iconButton
                        (IconButton.config
                            |> IconButton.setAttributes [ TopAppBar.actionItem ]
                        )
                        "account_box"
                    ]
                ]
            ]            
            
{--                 
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
                                [ text siteInfo.sitename ]                                                          
                            ],
                       IconButton.iconButton
                                  (IconButton.config
                                  |> IconButton.setAttributes
                                       [ TopAppBar.navigationIcon ]
                                  )
                                  "star"
                            , Html.span [ TopAppBar.actionItem ]
                                [ text siteInfo.fullname ]
                       ]                     
                      
                ]
                
--}
