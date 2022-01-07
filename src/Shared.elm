module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template)

import Browser.Navigation
import Css
import DataSource
import Footer
import Html as Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Attributes.Aria as Aria
import Html.Events as Event
import Html.Styled as Htmls
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events as Events
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import Svg.Styled exposing (path, svg)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as TwBp
import Tailwind.Utilities as Tw
import View exposing (View)


template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Just OnPageChange
    }


type Msg
    = OnPageChange
        { path : Path
        , query : Maybe String
        , fragment : Maybe String
        }
    | SharedMsg SharedMsg
    | ToggleMobileMenu
    | ToggleProfileMenu


type alias Data =
    ()


type SharedMsg
    = NoOp


type alias Model =
    { showMobileMenu : Bool
    , showProfileMenu : Bool
    }


init :
    Maybe Browser.Navigation.Key
    -> Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : Path
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Cmd Msg )
init navigationKey flags maybePagePath =
    ( { showMobileMenu = False
      , showProfileMenu = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPageChange _ ->
            ( { model | showMobileMenu = False }, Cmd.none )

        SharedMsg globalMsg ->
            ( model, Cmd.none )

        ToggleMobileMenu ->
            ( { model | showMobileMenu = not model.showMobileMenu }, Cmd.none )

        ToggleProfileMenu ->
            ( { model | showProfileMenu = not model.showProfileMenu }, Cmd.none )


subscriptions : Path -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


data : DataSource.DataSource Data
data =
    DataSource.succeed ()


view :
    Data
    ->
        { path : Path
        , route : Maybe Route
        }
    -> Model
    -> (Msg -> msg)
    -> View msg
    -> { body : Html msg, title : String }
view sharedData page model toMsg pageView =
    { body =
        div
            []
            ((myNav model
                |> Htmls.toUnstyled
                |> Html.map toMsg
             )
                :: List.append
                    pageView.body
                    [ indexViewFooter ]
            )
    , title = pageView.title
    }


indexViewFooter : Html msg
indexViewFooter =
    let
        viewPieNavega : List (Html msg)
        viewPieNavega =
            [ Footer.ligaAlPie "#" "About"
            , Footer.ligaAlPie "#" "Blog"
            , Footer.ligaAlPie "#" "Jobs"
            , Footer.ligaAlPie "#" "Press"
            , Footer.ligaAlPie "#" "Accesibility"
            , Footer.ligaAlPie "#" "Partners"
            ]

        viewPieSocialIcons : List (Html msg)
        viewPieSocialIcons =
            [ Footer.ligaIcono "github.com" "GitHub" Footer.Github
            , Footer.ligaIcono "linkedin.com" "LinkedIn" Footer.LinkedIn
            , Footer.ligaIcono "whatsapp.com" "Whatsapp" Footer.WhatsApp
            , Footer.ligaIcono "correo.com" "Correo" Footer.Email
            ]
    in
    Footer.viewFooter
        viewPieNavega
        viewPieSocialIcons
        "REFTEX INGENIERIA, S.A. de C.V. - 2021"


type alias Liga =
    { direccion : String
    , queDice : String
    }


myNav :
    Model
    -> Htmls.Html Msg
myNav modelo =
    let
        getMenu : List Liga
        getMenu =
            [ Liga "#one" "uno"
            , Liga "#two" "dos"
            , Liga "#three" "tres"
            ]

        myLogoAndLinks : Htmls.Html Msg
        myLogoAndLinks =
            Htmls.div
                [ css [ Tw.flex, Tw.items_center ] ]
                [ Htmls.div
                    -- EL LOGO
                    [ css [ Tw.flex_shrink_0 ] ]
                    [ Htmls.img
                        [ css [ Tw.h_8, Tw.w_8 ]
                        , Attrs.src "https://tailwindui.com/img/logos/workflow-mark-indigo-500.svg"
                        , Attrs.alt "Workflow"
                        ]
                        []
                    ]
                , Htmls.div
                    -- LIGAS DE NAVEGACION
                    [ css [ Tw.hidden, TwBp.md [ Tw.block ] ] ]
                    [ Htmls.div
                        [ css
                            [ Tw.ml_10
                            , Tw.flex
                            , Tw.items_baseline
                            , Tw.space_x_4
                            ]
                        ]
                        (ligasChulas False <| getMenu)
                    ]
                ]

        myHiddenMenu : Htmls.Html Msg
        myHiddenMenu =
            Htmls.div
                [ css <|
                    TwBp.md [ Tw.hidden ]
                        :: (if modelo.showMobileMenu then
                                [ Tw.block ]

                            else
                                [ Tw.hidden ]
                           )
                ]
                [ Htmls.div
                    [ css
                        [ Tw.px_2
                        , Tw.pt_2
                        , Tw.pb_3
                        , Tw.space_y_1
                        , TwBp.sm [ Tw.px_3 ]
                        ]
                    ]
                    (ligasChulas True <| getMenu)
                ]

        ligasChulas :
            Bool
            -> List Liga
            -> List (Htmls.Html Msg)
        ligasChulas esMovil menus =
            let
                clasesBase =
                    [ Tw.text_gray_300
                    , Css.hover [ Tw.bg_gray_700, Tw.text_white ]
                    , Tw.px_3
                    , Tw.py_2
                    , Tw.rounded_md
                    , Tw.font_medium
                    ]

                claseActual =
                    [ Tw.bg_gray_900
                    , Tw.text_white
                    , Tw.px_3
                    , Tw.py_2
                    , Tw.rounded_md
                    , Tw.font_medium
                    ]

                claseActualEnMovil =
                    [ Tw.block, Tw.text_base ]

                claseActualEnDesktop =
                    [ Tw.text_sm ]

                claseExtraPaMovil =
                    [ Tw.block, Tw.text_base ]

                claseExtraPaDesktop =
                    [ Tw.text_sm ]

                clasesQueVan esParaMovil liga =
                    -- if PagePath.toString page.path == liga.direccion then
                    if esParaMovil then
                        claseActual ++ claseActualEnMovil

                    else
                        claseActual ++ claseActualEnDesktop

                ligaChula clases liga =
                    Htmls.a
                        [ Attrs.href liga.direccion
                        , css clases
                        ]
                        [ Htmls.text liga.queDice ]
            in
            List.map
                (\algoDelMenu ->
                    ligaChula
                        (clasesQueVan esMovil algoDelMenu)
                        algoDelMenu
                )
                menus

        heroiconOutlineMenu : Htmls.Html Msg
        heroiconOutlineMenu =
            Htmls.div
                [ css
                    [ Tw.h_6, Tw.w_6, Tw.block ]
                ]
                [ svg
                    [ SvgAttr.fill "none"
                    , SvgAttr.viewBox "0 0 24 24"
                    , SvgAttr.stroke "currentColor"

                    -- aria-hidden="true"
                    ]
                    [ path
                        [ SvgAttr.strokeLinecap "round"
                        , SvgAttr.strokeLinejoin "round"
                        , SvgAttr.strokeWidth "2"
                        , SvgAttr.d "M4 6h16M4 12h16M4 18h16"
                        ]
                        []
                    ]
                ]

        heroiconOutlineX : Htmls.Html Msg
        heroiconOutlineX =
            Htmls.div
                [ css [ Tw.h_6, Tw.w_6, Tw.block ] ]
                [ svg
                    [ SvgAttr.fill "none"
                    , SvgAttr.viewBox "0 0 24 24"
                    , SvgAttr.stroke "currentColor"

                    -- aria-hidden="true"
                    ]
                    [ path
                        [ SvgAttr.strokeLinecap "round"
                        , SvgAttr.strokeLinejoin "round"
                        , SvgAttr.strokeWidth "2"
                        , SvgAttr.d "M6 18L18 6M6 6l12 12"
                        ]
                        []
                    ]
                ]

        mobileMenuButton : Htmls.Html Msg
        mobileMenuButton =
            Htmls.div
                [ css
                    [ Tw.neg_mr_2
                    , Tw.flex
                    , TwBp.md [ Tw.hidden ]
                    ]
                ]
                [ Htmls.button
                    [ css
                        [ Tw.bg_gray_800
                        , Tw.inline_flex
                        , Tw.items_center
                        , Tw.justify_center
                        , Tw.p_2
                        , Tw.rounded_md
                        , Tw.text_gray_400
                        , Css.hover
                            [ Tw.text_white
                            , Tw.bg_gray_700
                            ]
                        , Css.focus
                            [ Tw.outline_none
                            , Tw.ring_2
                            , Tw.ring_offset_2
                            , Tw.ring_offset_gray_800
                            , Tw.ring_white
                            ]
                        ]
                    , Events.onClick ToggleMobileMenu
                    ]
                    [ Htmls.span
                        [ css [ Tw.sr_only ] ]
                        [ Htmls.text "Open main menu" ]
                    , Htmls.span
                        []
                        [ if modelo.showMobileMenu then
                            heroiconOutlineX

                          else
                            heroiconOutlineMenu
                        ]
                    ]
                ]
    in
    Htmls.nav
        [ css [ Tw.bg_gray_800 ] ]
        [ Htmls.div
            [ css
                [ Tw.max_w_7xl
                , Tw.mx_auto
                , Tw.px_4
                , TwBp.lg [ Tw.px_8 ]
                , TwBp.sm [ Tw.px_6 ]
                ]
            ]
            [ Htmls.div
                [ css
                    [ Tw.flex
                    , Tw.items_center
                    , Tw.justify_between
                    , Tw.h_16
                    ]
                ]
                [ myLogoAndLinks
                , mobileMenuButton
                ]
            ]
        , myHiddenMenu
        ]
