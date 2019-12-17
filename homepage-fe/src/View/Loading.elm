module View.Loading exposing (view)

import Browser exposing (Document)
import Bootstrap.Spinner exposing (spinner)

view : Document msg
view =
    { title = "Loading..."
    , body =
        [ spinner [] []
        ]
    }