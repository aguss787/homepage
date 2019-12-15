module View.Profile exposing (view)

import Browser exposing (Document)
import Html exposing (img, text, div, a, ul, li)
import Html.Attributes exposing (src, href)
import Model.Profile as Profile

view : Profile.Model -> Document msg
view model =
    let
        imageUrl = model.imageUrl
    in
    { title = "asd"
    , body = [
           div [] [
               ul []
                   ( List.map
                       (\x -> li [] [a [ href ("http://localhost:8080/meme/" ++ x) ] [ text x ]])
                       imageUrl
                   )
           ]
       ]
    }