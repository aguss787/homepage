module View.Profile exposing (view)

import Browser exposing (Document)
import Css exposing (..)
import Html.Attributes exposing (style)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, class, id, target)
import Model.Profile as Profile
import Bootstrap.Grid as Grid
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Markdown
import Message
import Maybe
import Utils.Either exposing (..)

view : Profile.Model -> Document msg
view model =
    let
        profile = model.profile.profile
        educations = model.profile.educations
        projects = model.profile.projects

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
                    , width <| pct 75
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
                        [ div [] <| List.map fromUnstyled <| Markdown.toHtml Nothing profile.tagline
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
                                , div [] <| List.map fromUnstyled <| Markdown.toHtml Nothing profile.tagline
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

        educationRow = cardListRowView "Education"
            <| List.map (\x ->
                { title = x.institution
                , subtitle = Just x.info
                , duration = Just <| Right
                    { from = x.from
                    , until = x.until
                    }
                , content = div [] <| List.map fromUnstyled <| Markdown.toHtml Nothing x.description
                , picture = x.picture
                }) educations

        experienceRow = cardListRow "Experience"
                |> cardListRowImageHeight 150
                |> cardListRowImageWidth 150
                |> (cardListRowWithConfig
                    <| List.map (\x ->
                        { title = x.name
                        , subtitle = Just x.info
                        , duration = Just <| Right
                            { from = x.from
                            , until = x.until
                            }
                        , content = div [] <| List.map fromUnstyled <| Markdown.toHtml Nothing x.description
                        , picture = x.picture
                        }) model.profile.experiences
                   )

        achievementRow = cardListRow "Achievement"
                |> cardListRowImageHeight 150
                |> cardListRowImageWidth 150
                |> (cardListRowWithConfig
                    <| List.map (\x ->
                        { title = x.name
                        , subtitle = Nothing
                        , duration = Just <| Left x.date
                        , content = div [] <| List.map fromUnstyled <| Markdown.toHtml Nothing x.description
                        , picture = x.picture
                        }) model.profile.achievements
                   )

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
                    , experienceRow
                    , achievementRow
                    ]
                ]
            ]
        }

type alias Duration =
    { from : String
    , until : String
    }

type alias CardListData =
    { title : String
    , subtitle : Maybe String
    , duration : Maybe (Either String Duration)
    , content : Html Message.Message
    , picture : String
    }

type alias CardListConfig =
    { imageWidth : Int
    , imageHeight : Int
    , header : String
    }

cardListHorizontal conf data = div
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
                              , height <| px conf.imageHeight
                              , width <| px conf.imageWidth
                              ]
                          ] []
                    ]
                , toUnstyled <| div
                    [ css
                        [ marginLeft <| px <| conf.imageWidth + 15
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
                            [ text data.title ]
                        , renderSubtitle data.subtitle
                        , renderDate data.duration
                        ]
                    , div [] [ data.content ]
                    ]
                ]
            ]
        |> Card.view
        |> fromUnstyled
    ]

cardListVertical conf data = div
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
                              , height <| px conf.imageHeight
                              , width <| px conf.imageWidth
                              ]
                          ] []
                    , h4 [] [ text data.title ]
                    , renderSubtitle data.subtitle
                    , renderDate data.duration
                    ]
                , hr [] []
                , div []
                    [ div [] [ data.content ]
                    ]
                ]
            ] ]
        |> Card.view
        |> fromUnstyled
    ]

renderSubtitle str = Maybe.withDefault (div [] []) <| Maybe.map
    ( \x ->
        div
              [ css
                  [ fontSize <| px 15
                  ]
              ]
              [ text x ]
    ) str

renderDate date =
    let
        dateText text = unify
                   <| mapRight (\x -> x.from ++ " - " ++ x.until)
                   <| text
    in
        Maybe.withDefault (div [] []) <| Maybe.map
            (\x -> div
                 [ css
                     [ fontSize <| px 13
                     ]
                 ] [ text <| dateText x ]
              ) date

cardListRowWithConfig data conf = Grid.row []
    [ Grid.col []
        [ toUnstyled <| div
            [ id conf.header
            , css
                [ marginTop <| px 10
                , textAlign center
                ]
            ]
            [ div []
                [ h2 []
                    [ text conf.header ]
                ]
            ]
        , toUnstyled <| div []
            <| List.map (cardListHorizontal conf) data
            ++ List.map (cardListVertical conf) data
        ]
    ]

cardListRowView header data = cardListRowWithConfig data <| cardListRow header

cardListRow header =
    { imageHeight = 150
    , imageWidth = 120
    , header = header
    }

cardListRowImageWidth val conf = { conf | imageWidth = val }
cardListRowImageHeight val conf = { conf | imageHeight = val }