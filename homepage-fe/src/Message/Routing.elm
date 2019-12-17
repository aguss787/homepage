module Message.Routing exposing (..)

import Url exposing (Url)

type Message
    = ExternalRequest String
    | InternalRequest Url