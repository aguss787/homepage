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

type alias Experience =
    { name : String
    , info : String
    , from : String
    , until : String
    , description : String
    , picture : String
    }

experienceDecoder = map6 Experience
    (field "experienceName" string)
    (field "experienceInfo" string)
    (field "experienceFrom" string)
    (field "experienceUntil" string)
    (field "experienceDescription" string)
    (field "experiencePicture" string)


type alias Achievement =
    { name : String
    , date : String
    , description : String
    , picture : String
    }

achievementDecoder = map4 Achievement
    (field "achievementName" string)
    (field "achievementDate" string)
    (field "achievementDescription" string)
    (field "achievementPicture" string)

type alias CompleteProfile =
    { profile : Profile
    , educations : List Education
    , projects : List Project
    , experiences : List Experience
    , achievements : List Achievement
    }

completeProfileDecoder = map5 CompleteProfile
    (field "profile" profileDecoder)
    (field "educations" <| list educationDecoder)
    (field "projects" <| list projectDecoder)
    (field "experiences" <| list experienceDecoder)
    (field "achievements" <| list achievementDecoder)

type alias Model =
    { profile : CompleteProfile
    }
