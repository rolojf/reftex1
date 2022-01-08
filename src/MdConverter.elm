module MdConverter exposing (renderea)

import Html as Html exposing (Html, div, text)
import Markdown.Parser
import Markdown.Renderer exposing (defaultHtmlRenderer)


deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.Parser.deadEndToString
        |> String.join "\n"


renderea : String -> Html Never
renderea markdown =
    div
        []
        [ case
            markdown
                |> Markdown.Parser.parse
                |> Result.mapError deadEndsToString
                |> Result.andThen
                    (\ast ->
                        Markdown.Renderer.render
                            defaultHtmlRenderer
                            ast
                    )
          of
            Ok rendereado ->
                div [] rendereado

            Err errors ->
                text errors
        ]
