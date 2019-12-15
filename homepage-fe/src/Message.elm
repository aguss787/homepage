module Message exposing (..)

import Message.Routing as Routing
import Message.Profile as Profile

type Message
    = ProfileMessage Profile.Message
    | RoutingMessage Routing.Message
    | None