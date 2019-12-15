module Model exposing (..)

import Browser.Navigation as Navigation
import Model.Profile as Profile
import Page exposing (Page)

type alias SpecificModel =
    { profile : Profile.Model
    }

emptySpecificModel : SpecificModel
emptySpecificModel =
    { profile = Profile.emptyModel
    }

type alias Model =
    { navKey : Navigation.Key
    , model : SpecificModel
    , page : Page
    }