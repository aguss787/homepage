module Message.Profile exposing (..)

import Http
import Model.Profile as Profile

type Message
    = GotResult (Result Http.Error Profile.CompleteProfile)