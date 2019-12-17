module Init exposing (init)

import Model exposing (Model, SpecificModel)
import Message exposing (Message)
import Url exposing (Url)
import Browser.Navigation as Navigation
import Page as Page
import Update.Profile as Profile
import Model.Profile as Profile
import Bootstrap.Navbar as Navbar
import Message.Container as Container

navbarModel : Navbar.State
navbarModel =
    let
        (state, _) = Navbar.initialState (\x -> Message.ContainerMessage <| Container.NavbarMsg x)
    in
        state

navbarCmd : Cmd Message.Message
navbarCmd =
    let
        (_, cmd) = Navbar.initialState (\x -> Message.ContainerMessage <| Container.NavbarMsg x)
    in
        cmd

emptySpecificModel : SpecificModel
emptySpecificModel =
    { container =
        { navbar = navbarModel
        }
    , profile =
        { profile =
            { name = ""
            , picture = ""
            , tagline = ""
            }
        }
    }

init : () -> Url -> Navigation.Key -> ( Model, Cmd Message )
init _ url key =
    ( {navKey = key, model = emptySpecificModel, page = Page.Loading}
    , Cmd.batch
        [ navbarCmd
        , Profile.fetchCatImageUrl
        ]
    )
