module MdConverter exposing (renderea)

import Html as Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Markdown.Html
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
                            myRenderer
                            ast
                    )
          of
            Ok rendereado ->
                div [] rendereado

            Err errors ->
                text errors
        ]


myRenderer : Markdown.Renderer.Renderer (Html Never)
myRenderer =
    let
        defaultOne =
            Markdown.Renderer.defaultHtmlRenderer
    in
    { defaultOne | html = htmls }



-- showDiv : String -> String -> Html Never


showDiv : Maybe String -> Maybe String -> List (Html Never) -> Html Never
showDiv clase quienSoy children =
    div
        [ case clase of
            Just claseDef ->
                class claseDef

            Nothing ->
                class ""
        , case quienSoy of
            Just soyYoMero ->
                Attr.id soyYoMero

            Nothing ->
                Attr.id ""
        ]
        children


showSpan : String -> List (Html Never) -> Html Never
showSpan clase children =
    div
        [ class clase ]
        children


htmls : Markdown.Html.Renderer (List (Html Never) -> Html Never)
htmls =
    Markdown.Html.oneOf
        [ Markdown.Html.tag "div"
            showDiv
            |> Markdown.Html.withOptionalAttribute "class"
            |> Markdown.Html.withOptionalAttribute "id"
        , Markdown.Html.tag "span"
            showSpan
            |> Markdown.Html.withAttribute "class"
        ]
