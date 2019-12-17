module Model exposing (..)

import Browser.Navigation as Navigation
import Model.Profile as Profile
import Model.Container as Container
import Page exposing (Page)

type alias SpecificModel =
    { container : Container.Model
    , profile : Profile.Model
    }

type alias Model =
    { navKey : Navigation.Key
    , model : SpecificModel
    , page : Page
    }