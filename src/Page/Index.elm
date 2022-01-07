module Page.Index exposing (Data, Model, Msg, page)

import DataSource exposing (DataSource)
import DataSource.File as File
import Folders
import Head
import Head.Seo as Seo
import Html as Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Route exposing (Route)
import Shared
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    {}


page : Page RouteParams Data
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


data : DataSource Data
data =
    let
        fnJalaElMDyConv : Folders.Reporte -> DataSource Entry
        fnJalaElMDyConv { slug, filePath } =
            File.bodyWithFrontmatter
                mdDecoder
                filePath
                |> DataSource.map
                    (fnConvMDaEntry slug)

        mdDecoder : String -> Decoder { title : String, body : String }
        mdDecoder body =
            Decode.map (\title -> { title = title, body = body })
                (Decode.field "title" Decode.string)

        fnConvMDaEntry : String -> { title : String, body : String } -> Entry
        fnConvMDaEntry slug { title, body } =
            { title = title
            , route = Route.Tab_ { tab = slug }
            , excerpt = String.left 80 body ++ "..."
            }
    in
    Folders.all
        |> DataSource.andThen
            (List.map
                fnJalaElMDyConv
                >> DataSource.combine
            )


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


type alias Data =
    List Entry


type alias Entry =
    { title : String
    , route : Route
    , excerpt : String
    }


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = "Listado Matón"
    , body = List.map viewEntry static.data
    , menu =
        [ View.Liga "#one" "uno"
        , View.Liga "#two" "dos"
        , View.Liga "#three" "tres"
        ]
    }


viewEntry : Entry -> Html msg
viewEntry entry =
    Html.article []
        [ Route.link entry.route [] [ Html.text entry.title ] ]
