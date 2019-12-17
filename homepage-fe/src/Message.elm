module Message exposing (..)

import Message.Routing as Routing
import Message.Profile as Profile
import Message.Container as Container

type Message
    = ProfileMessage Profile.Message
    | ContainerMessage Container.Message
    | RoutingMessage Routing.Message
    | None