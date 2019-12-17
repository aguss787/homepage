module Update.Container exposing (update)

import Message.Container as Container
import Model exposing (Model)

update : Container.Message -> Model -> (Model, Cmd msg)
update msg model =
    let
        oldModel = model.model
        oldContainer = oldModel.container
    in
        case msg of
            Container.NavbarMsg state ->
                ( { model
                  | model =
                      { oldModel
                      | container =
                          { oldContainer
                          | navbar = state
                          }
                      }
                  }, Cmd.none )