module Main exposing (..)

import Html
import Model
import Update
import View


main =
    Html.beginnerProgram
        { update = Update.update, view = View.view, model = Model.model }
