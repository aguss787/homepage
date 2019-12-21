module View.Profile exposing (view)

import Browser exposing (Document)
import Css exposing (..)
import Html.Attributes exposing (style)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, class, id, target)
import Model.Profile as Profile
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Utilities.Spacing as Spacing
import Markdown

view : Profile.Model -> Document msg
view model =
    let
        profile = model.profile.profile
        educations = model.profile.educations
        projects = model.profile.projects

        name separatorLength = div
            []
            [ text profile.name
            , hr [ css [ width <| pct separatorLength ] ] []
            , text profile.tagline
            ]
        horizontalName = div [class "d-none d-sm-block"] [ div
            [ css
                [ verticalAlign middle
                , height <| pct 100
                , width <| pct 100
                , display Css.table
                , marginTop <| px 10
                ]
            ]
            [ div
                [ css
                    [ display tableCell
                    , verticalAlign middle
                    , width <| pct 50
                    ]
                ]
                [ div
                    []
                    [ div
                        [ css
                            [ fontSize <| px 24
                            , fontWeight bolder
                            ]
                        ]
                        [ text profile.name
                        ]
                    , hr [ css [ width <| pct 100 ] ] []
                    , div
                        [ css
                            [ fontFamilies ["monospace"]
                            ]
                        ]
                        [ text profile.tagline
                        ]
                    ]
                ]
            , img
                [ src profile.picture
                , css
                    [ borderRadius <| pct 50
                    , height <| px 150
                    , position relative
                    , float right
                    ]
                ] []
            ] ]

        verticalName = div [class "d-block d-sm-none"]
            [ fromUnstyled <| Grid.containerFluid []
                [ Grid.simpleRow
                    [ Grid.col []
                        [ toUnstyled <| div
                            [ css
                                [ height <| pct 100
                                , width <| pct 100
                                , Css.textAlign center
                                ]
                            ]
                            [ img
                                [ src profile.picture
                                , css
                                    [ borderRadius <| pct 50
                                    , height <| px 150
                                    , margin <| px 10
                                    ]
                                ] []
                            ]
                        ]
                    ]
                , Grid.simpleRow
                    [ Grid.col []
                        [ toUnstyled <| div
                            [ css
                                [ height <| pct 100
                                , width <| pct 100
                                , marginTop <| px 10
                                , Css.textAlign center
                                ]
                            ]
                            [ div
                                []
                                [ div
                                    [ css
                                        [ fontSize <| px 24
                                        , fontWeight bolder
                                        ]
                                    ]
                                    [ text profile.name
                                    ]
                                , hr [ css [ width <| pct 50 ] ] []
                                , text profile.tagline
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        nameRow = Grid.row []
            [ Grid.col []
                [ toUnstyled <| div
                    [ id "profile"
                    ]
                    [ horizontalName
                    , verticalName
                    ]
                ]
            ]
        educationCardHorizontal data = div
            [ class "d-none d-sm-block"
            , css
                [ marginBottom <| px 10
                ]
            ]
            [ Card.config []
                |> Card.block []
                    [ Block.text []
                        [ toUnstyled <| div
                            [ css
                                [ position relative
                                , float left
                                , height <| pct 100
                                ]
                            ]
                            [ img
                                  [ src data.picture
                                  , css
                                      [ borderRadius <| pct 5
                                      , height <| px 150
                                      , width <| px 120
                                      ]
                                  ] []
                            ]
                        , toUnstyled <| div
                            [ css
                                [ marginLeft <| px 135
                                ]
                            ]
                            [ div
                                [ css [ marginBottom <| px 10 ]
                                ]
                                [ h4
                                    [ css
                                        [ marginBottom <| px 0
                                        ]
                                    ]
                                    [ text data.institution ]
                                , div
                                    [ css
                                        [ fontSize <| px 15
                                        ]
                                    ]
                                    [ text data.info ]
                                , div
                                    [ css
                                        [ fontSize <| px 13
                                        ]
                                    ]
                                    [ text <| data.from ++ " - " ++ data.until ]
                                ]
                            , div []
                                <| List.map fromUnstyled <| Markdown.toHtml Nothing data.description
                            ]
                        ]
                    ]
                |> Card.view
                |> fromUnstyled
            ]

        educationCardVertical data = div
            [ class "d-block d-sm-none"
            , css
                [ marginBottom <| px 10
                ]
            ]
            [ Card.config []
                |> Card.block []
                    [ Block.text [] [ toUnstyled <| div []
                        [ div
                            [ css
                                [ Css.textAlign center
                                ]
                            ]
                            [ img
                                  [ src <| data.picture
                                  , css
                                      [ borderRadius <| pct 5
                                      , height <| px 150
                                      , width <| px 120
                                      ]
                                  ] []
                            , h4 [] [ text data.institution ]
                            , div
                                  [ css
                                      [ fontSize <| px 15
                                      ]
                                  ]
                                  [ text data.info ]
                            , div
                                  [ css
                                      [ fontSize <| px 13
                                      ]
                                  ]
                                  [ text <| data.from ++ " - " ++ data.until ]
                            ]
                        , hr [] []
                        , div []
                            [ div []
                                <| List.map fromUnstyled <| Markdown.toHtml Nothing data.description
                            ]
                        ]
                    ] ]
                |> Card.view
                |> fromUnstyled
            ]

        educationRow = Grid.row []
            [ Grid.col []
                [ toUnstyled <| div
                    [ id "education"
                    , css
                        [ marginTop <| px 10
                        , textAlign center
                        ]
                    ]
                    [ div []
                        [ h2 []
                            [ text "Education"]
                        ]
                    ]
                , toUnstyled <| div []
                    <| List.map educationCardHorizontal educations
                    ++ List.map educationCardVertical educations
                ]
            ]

        projectCard data =
            let
                logoWidth = rem 15
            in
                a
                    [ href data.link
                    , Html.Styled.Attributes.target "_blank"
                    , css
                        [ color inherit
                        ]
                    ]
                    [ div
                        [ class "d-none d-sm-inline-block"
                        , css
                            [ height <| px 450
                            , width <| px 300
                            , overflow hidden
                            , display inlineBlock
                            , marginLeft <| px 20
                            , marginBottom <| px 10
                            ]
                        ]
                        [ Card.config [ Card.attrs [ style "height" "100%" ]  ]
                            |> Card.header [ style "height" "500px" ]
                                [ toUnstyled <| img
                                    [ src data.logo
                                    , css
                                        [ width logoWidth
                                        ]
                                    ] []
                                , toUnstyled <| h3 [ ] [ text data.name ]
                                ]
                            |> Card.block [ Block.attrs [ style "height" "100%" ] ]
                                [ Block.text [] [ toUnstyled <| text data.description ]
                                ]
                            |> Card.view
                            |> fromUnstyled
                    ]
                ]

        projectCardWide data =
            let
                logoWidth = rem 15
            in
                a
                [ href data.link
                , Html.Styled.Attributes.target "_blank"
                , css
                    [ color inherit
                    ]
                ]
                [ div
                    [ class "d-block d-sm-none"
                    , css
                        [ height <| px 450
                        , width <| pct 100
                        , overflow hidden
                        , display inlineBlock
                        , marginBottom <| px 10
                        ]
                    ]
                    [ Card.config [ Card.attrs [ style "height" "100%" ]  ]
                        |> Card.header [ style "height" "500px" ]
                            [ toUnstyled <| img
                                [ src data.logo
                                , css
                                    [ width logoWidth
                                    ]
                                ] []
                            , toUnstyled <| h3 [ ] [ text data.name ]
                            ]
                        |> Card.block [ Block.attrs [ style "height" "100%" ] ]
                            [ Block.text [] [ toUnstyled <| text data.description ]
                            ]
                        |> Card.view
                        |> fromUnstyled
                    ]
                ]

        projectRow = Grid.row []
            [ Grid.col []
                [ toUnstyled <| div
                    [ id "project"
                    , css
                        [ marginTop <| px 10
                        , textAlign center
                        ]
                    ]
                    [ div []
                        [ h2 []
                            [ text "Projects" ]
                        ]
                    ]
                , toUnstyled <| div
                    [ css
                        [ textAlign center
                        ]
                    ]
                    <| List.map projectCard projects
                    ++ List.map projectCardWide projects
                ]
            ]
    in
        { title = "Profile"
        , body =
            [ toUnstyled <| div
                [ css
                    [ fontFamilies ["-apple-system"
                                   ,"system-ui"
                                   ,"BlinkMacSystemFont"
                                   ,"Segoe UI"
                                   ,"Roboto"
                                   ,"Helvetica Neue"
                                   ,"Fira Sans"
                                   ,"Ubuntu"
                                   ,"Oxygen"
                                   ,"Oxygen Sans"
                                   ,"Cantarell"
                                   ,"Droid Sans"
                                   ,"Apple Color Emoji"
                                   ,"Segoe UI Emoji"
                                   ,"Segoe UI Symbol"
                                   ,"Lucida Grande"
                                   ,"Helvetica"
                                   ,"Arial"
                                   ,"sans-serif"
                                   ]
                    ]
                ]
                [ fromUnstyled <| Grid.containerFluid []
                    [ nameRow
                    , toUnstyled <| hr [] []
                    , educationRow
                    , projectRow
                    ]
                ]
            ]
        }
