module Page.Tab_ exposing (Data, Model, Msg, page)

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
    DataSource.map
        (List.map
            (\reporte -> RouteParams reporte.slug)
        )
        Folders.all


data : RouteParams -> DataSource Data
data routeParams =
    let
        mdyBDecoder : String -> Decoder { body : String, title : String }
        mdyBDecoder cuerpo =
            Decode.map
                (\title -> { body = cuerpo, title = title })
                (Decode.field "title" Decode.string)
    in
    ("data/" ++ routeParams.tab ++ "/notas.md")
        |> File.bodyWithFrontmatter mdyBDecoder
        |> DataSource.map
            (\{ body, title } ->
                { body = MdConverter.renderea body
                , title = title
                }
            )


type alias Data =
    { body : Html Never, title : String }


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
    { title = "Componente Revisado"
    , body =
        [ static.data.body
        , text ("Tab_" ++ static.routeParams.tab)
        ]
    , menu =
        [ View.Liga "#aaa" "AAA"
        , View.Liga "#bbb" "BBB"
        , View.Liga "#ccc" "CCC"
        ]
    }
