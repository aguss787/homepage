module Message.Profile exposing (..)

import Http

type Message
    = GotResult (Result Http.Error (List String))