module View.Container exposing (..)

import Browser exposing (Document)
import Utils.View.Adaptor exposing (Adaptor)

container : Adaptor model msg
container previous =
    let
        doc = previous.result
        body = doc.body
        template x = x
    in
        { previous
        | result = { doc
                   | body = template body
                   }
        }
