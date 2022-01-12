module Page.Tab_ exposing (Data, Model, Msg, page)

import Array exposing (Array)
import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import Folders
import Head
import Head.Seo as Seo
import Html as Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import MdConverter
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Route
import Shared
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    { tab : String }


page : Page RouteParams Data
page =
    Page.prerender
        { head = head
        , routes = routes
        , data = data
        }
        |> Page.buildNoState { view = view }


routes : DataSource (List RouteParams)
routes =
    Folders.all
        |> DataSource.map
            (List.map
                (\reporte -> RouteParams reporte.slug)
            )


type alias BodyAndFrontMatter =
    { body : Html Never, title : String, fotos : Int, apunte : Bool }


data : RouteParams -> DataSource Data
data routeParams =
    let
        -- mdyBDecoder : String -> Decoder FrontMatter
        mdyBDecoder cuerpo =
            Decode.map3 (BodyAndFrontMatter (MdConverter.renderea cuerpo))
                (Decode.field "title" Decode.string)
                (Decode.field "foto" Decode.int)
                (Decode.field "apunte" Decode.bool)

        getDataFromMD =
            ("public/" ++ routeParams.tab ++ "/notas.md")
                |> File.bodyWithFrontmatter mdyBDecoder

        getReportes =
            DataSource.map
                Array.fromList
                Folders.all

        --                     (List.sortBy .slug)
    in
    DataSource.map2 Data
        getDataFromMD
        getReportes


type alias Data =
    { fMatter : BodyAndFrontMatter
    , tabs : Array Folders.Reporte
    }


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    let
        indiceActual : Int
        indiceActual =
            static.data.tabs
                |> Array.indexedMap
                    (\indice { slug } ->
                        if slug == static.routeParams.tab then
                            indice

                        else
                            0
                    )
                |> Array.foldl (+) 0

        indToSlug : Int -> String
        indToSlug ind =
            static.data.tabs
                |> Array.get ind
                |> Maybe.map .slug
                |> Maybe.withDefault "error"

        nextInd : Int
        nextInd =
            if indiceActual >= Array.length static.data.tabs - 1 then
                0

            else
                indiceActual + 1

        prevInd : Int
        prevInd =
            if indiceActual <= 0 then
                Array.length static.data.tabs - 1

            else
                indiceActual - 1
    in
    { title = "Componente Revisado"
    , body =
        [ div
            [ class "mt-6" ]
            [ div
                [ class "md:flex md:gap-4" ]
                [ Html.img
                    [ Attr.src <| static.routeParams.tab ++ "/1.jpg"
                    , Attr.alt <| "Foto de " ++ static.data.fMatter.title
                    , Attr.width 300
                    ]
                    []
                , if static.data.fMatter.fotos >= 2 then
                    Html.img
                        [ class "mt-4"
                        , Attr.src <| static.routeParams.tab ++ "/2.jpg"
                        , Attr.alt <| "Foto de " ++ static.data.fMatter.title
                        , Attr.width 300
                        ]
                        []

                  else
                    div [] []
                , if static.data.fMatter.fotos == 3 then
                    Html.img
                        [ class "mt-4"
                        , Attr.src <| static.routeParams.tab ++ "/3.jpg"
                        , Attr.alt <| "Foto de " ++ static.data.fMatter.title
                        , Attr.width 300
                        ]
                        []

                  else
                    div [] []
                , if static.data.fMatter.apunte then
                    div
                        [ class "border-2 border-gray-500" ]
                        [ Html.img
                            [ Attr.src <| static.routeParams.tab ++ "/a.png"
                            , Attr.alt <| "Foto de " ++ static.data.fMatter.title
                            , Attr.width 500
                            ]
                            []
                        ]

                  else
                    div [] []
                ]
            , div
                [ class "prose lg:prose-xl" ]
                [ static.data.fMatter.body ]
            ]
        , text ("Tab_" ++ static.routeParams.tab)
        , indiceActual
            |> Debug.toString
            |> text
            |> List.singleton
            |> div []
        ]
    , menu =
        [ View.Liga
            Route.Index
            "Inicio"
        , View.Liga
            (Route.Tab_ { tab = indToSlug prevInd })
            ("Previo: " ++ indToSlug prevInd)
        , View.Liga
            (Route.Tab_ { tab = indToSlug nextInd })
            ("Siguiente: " ++ indToSlug nextInd)
        ]
    }
