module Model.Profile exposing
    ( Profile
    , CompleteProfile
    , completeProfileDecoder
    , Model
    )

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

type alias Education =
    { institution : String
    , info : String
    , from : String
    , until : String
    , description : String
    , picture : String
    }

educationDecoder = map6 Education
    (field "educationInstitution" string)
    (field "educationInfo" string)
    (field "educationFrom" string)
    (field "educationUntil" string)
    (field "educationDescription" string)
    (field "educationPicture" string)

type alias Project =
    { name : String
    , description : String
    , logo : String
    , link : String
    }

projectDecoder = map4 Project
    (field "projectName" string)
    (field "projectDescription" string)
    (field "projectLogo" string)
    (field "projectLink" string)

type alias CompleteProfile =
    { profile : Profile
    , educations : List Education
    , projects : List Project
    }

completeProfileDecoder = map3 CompleteProfile
    (field "profile" profileDecoder)
    (field "educations" <| list educationDecoder)
    (field "projects" <| list projectDecoder)

type alias Model =
    { profile : CompleteProfile
    }
