module View.Container exposing (templateContainer)

import Browser exposing (Document)
import Html exposing (Html, text, node)
import Html.Attributes exposing (href, rel)
import Html.Styled exposing (div, toUnstyled, fromUnstyled)
import Html.Styled.Attributes exposing (css)
import Css exposing (..)
import Utils.View.Adaptor exposing (Adaptor)
import Bootstrap.Grid exposing (container, simpleRow, col)
import Bootstrap.Navbar as Navbar
import Message as Message
import Message.Container as Container
import Model exposing (Model)
import Message exposing (Message)
import Color

templateContainer : Adaptor Model Message
templateContainer previous =
    let
        model = previous.model
        doc = previous.result
        body = doc.body
    in
        { previous
        | result = { doc
                   | body = template model body
                   }
        }

cssLink str =
    node "link"
        [ rel "stylesheet"
        , href str
        ]
        []

template : Model -> List (Html Message) -> List (Html Message)
template model content =
    [ cssLink "https://cdn.agus.dev/bootstrap.min.css"
    , cssLink "https://cdn.agus.dev/font-awesome.min.css"
    , container []
        [ Navbar.config (\x -> Message.ContainerMessage <| Container.NavbarMsg x)
          |> Navbar.withAnimation
          |> Navbar.fixTop
          |> Navbar.lightCustom Color.white
          |> Navbar.brand [ href "/#"] [ text "Agus.dev"]
          |> Navbar.items
              [ Navbar.itemLink [href "https://cdn.agus.dev/resume.mp4"]  [ text "Resume"]
              ]
          |> Navbar.view model.model.container.navbar
        , simpleRow
            [ col []
                [ toUnstyled <| div
                    [ css
                        [ paddingTop <| px 50
                        ]
                    ]
                    <| List.map fromUnstyled content

                ]
            ]
        ]
    ]

