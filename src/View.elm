module View exposing (Liga, View, map, placeholder)

import Html
import Route


type alias Liga =
    { direccion : Route.Route
    , queDice : String
    }


type alias View msg =
    { title : String
    , body : List (Html.Html msg)
    , menu : List Liga
    }


map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn doc =
    { title = doc.title
    , body = List.map (Html.map fn) doc.body
    , menu = doc.menu
    }


placeholder : String -> View msg
placeholder moduleName =
    { title = "Placeholder - " ++ moduleName
    , body = [ Html.text moduleName ]
    , menu = []
    }
