module Model.Container exposing (Model)

import Browser exposing (..)
import Bootstrap.Navbar as Navbar
import Message.Container as Container
import Message as Message

type alias Model =
    { navbar: Navbar.State
    }
