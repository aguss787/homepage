module Utils.View.Adaptor exposing (..)

import Browser exposing (Document)

type alias AdaptorResult model msg =
    { model : model
    , result : Document msg
    }

type alias Adaptor model msg = AdaptorResult model msg -> AdaptorResult model msg

toAdaptor : model -> Document msg -> AdaptorResult model msg
toAdaptor model doc =
    { model = model
    , result = doc
    }

fromAdaptor : AdaptorResult model msg -> Document msg
fromAdaptor = .result