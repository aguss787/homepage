module Model.Profile exposing (Profile, profileDecoder, Model)

import Json.Decode exposing (..)

type alias Profile =
    { name : String
    , picture : String
    , tagline : String
    }

profileDecoder = map3 Profile
    (field "profileName" string)
    (field "profilePicture" string)
    (field "profileTagline" string)

type alias Model =
    { profile : Profile
    }
