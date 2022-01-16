module MdConverter exposing (renderea)

import Html exposing (Html, div, text)
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
    { defaultOne | html = fnProcHtml }


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
    let
        procesa1stChild child = [ child ]
            {-case child of
                (Html.p atributos conte) -> List.singleton (Html.p atributos conte)
                otroChild -> List.singleton otroChild-}

    in
    Html.span
        [ class clase ]
        (case children of
            x :: rest ->
                 (List.append
                     (procesa1stChild x)
                     rest
                 )
            [] -> []
        )



fnProcHtml : Markdown.Html.Renderer (List (Html Never) -> Html Never)
fnProcHtml =
    Markdown.Html.oneOf
        [ Markdown.Html.tag "div"
            showDiv
            |> Markdown.Html.withOptionalAttribute "class"
            |> Markdown.Html.withOptionalAttribute "id"
        , Markdown.Html.tag "span"
            showSpan
            |> Markdown.Html.withAttribute "class"
        ]
