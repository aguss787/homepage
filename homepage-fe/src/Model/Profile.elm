module Model.Profile exposing (..)

type alias Model =
    { imageUrl : List String
    }

emptyModel : Model
emptyModel =
    { imageUrl = []
    }