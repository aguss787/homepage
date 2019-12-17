module View.Profile exposing (view)

import Browser exposing (Document)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Model.Profile as Profile
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col exposing (..)

view : Profile.Model -> Document msg
view model =
    let
        name = div
            [ css
                [ display tableCell
                , verticalAlign middle
                , width <| pct 100
                ]
            ]
            [ text model.profile.name
            , hr [] []
            , text model.profile.tagline
            ]
        nameRow = Grid.row []
            [ Grid.col []
                [ toUnstyled <| div
                    [ css
                        [ verticalAlign middle
                        , height <| pct 100
                        , width <| pct 100
                        , display Css.table
                        , marginTop <| px 10
                        ]
                    ]
                    [ name
                    , img
                        [ src model.profile.picture
                        , css
                            [ borderRadius <| pct 50
                            , height <| px 150
                            , position relative
                            , float right
                            ]
                        ] []
                    ]
                ]
            ]
    in
        { title = "Profile"
        , body =
            [ Grid.container []
                [ nameRow
                ]
            ]
        }
