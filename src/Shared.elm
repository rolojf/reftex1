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

        {- Para cuando haya un menú de auth y según cada perfil
           myHiddenBlock : Htmls.Html Msg
           myHiddenBlock =
               Htmls.div
                   [ css
                       [ Tw.hidden
                       , TwBp.md [ Tw.block ]
                       ]
                   ]
                   [ Htmls.div
                       [ css
                           [ Tw.ml_4
                           , Tw.flex
                           , Tw.items_center
                           , TwBp.md [ Tw.ml_6 ]
                           ]
                       ]
                       [ Htmls.div
                           -- Profile dropdown --
                           [ css
                               [ Tw.ml_3
                               , Tw.relative
                               ]
                           ]
                           [ Htmls.div
                               []
                               [ Htmls.button
                                   [ css
                                       [ Tw.max_w_xs
                                       , Tw.bg_gray_800
                                       , Tw.rounded_full
                                       , Tw.flex
                                       , Tw.items_center
                                       , Tw.text_sm
                                       , Tw.text_white
                                       , Css.focus
                                           [ Tw.outline_none
                                           , Tw.ring_2
                                           , Tw.ring_offset_2
                                           , Tw.ring_offset_gray_800
                                           , Tw.ring_white
                                           ]
                                       ]
                                   , Attrs.id "user-menu"
                                   , Aria.ariaHasPopup "true" |> Attrs.fromUnstyled
                                   , Events.onClick ToggleProfileMenu
                                   ]
                                   [ Htmls.span
                                       [ css [ Tw.sr_only ] ]
                                       [ Htmls.text "Open user menu" ]
                                   , Htmls.img
                                       [ css
                                           [ Tw.h_8
                                           , Tw.w_8
                                           , Tw.rounded_full
                                           ]
                                       , Attrs.src "https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixqx=g09zpRVLoT&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80"
                                       , Attrs.alt ""
                                       ]
                                       []
                                   ]
                               ]
                           , if modelo.showProfileMenu then
                               Htmls.div
                                   [ css
                                       [ Tw.origin_top_right
                                       , Tw.absolute
                                       , Tw.right_0
                                       , Tw.mt_2
                                       , Tw.w_48
                                       , Tw.rounded_md
                                       , Tw.shadow_lg
                                       , Tw.py_1
                                       , Tw.bg_white
                                       , Tw.ring_1
                                       , Tw.ring_black
                                       , Tw.ring_opacity_5
                                       ]
                                   , Aria.role "menu" |> Attrs.fromUnstyled

                                   -- , Aria.aria-orientation "vertical"
                                   , Aria.ariaLabelledby "user-menu" |> Attrs.fromUnstyled
                                   ]
                                   (menuItems
                                       [ Tw.block
                                       , Tw.px_4
                                       , Tw.py_2
                                       , Tw.text_sm
                                       , Tw.text_gray_700
                                       , Css.hover [ Tw.bg_gray_100 ]
                                       ]
                                   )

                             else
                               Htmls.i [] []
                           ]
                       ]
                   ]
        -}
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

                {- , Htmls.div
                   [ css
                       [ Tw.pt_4
                       , Tw.pb_3
                       , Tw.border_t
                       , Tw.border_gray_700
                       ]
                   ]
                   [ Htmls.div
                       [ css
                           [ Tw.flex
                           , Tw.items_center
                           , Tw.px_5
                           ]
                       ]
                       [ Htmls.div
                           [ css [ Tw.flex_shrink_0 ] ]
                           [ Htmls.img
                               [ css [ Tw.h_10, Tw.w_10, Tw.rounded_full ]
                               , Attrs.src "https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixqx=g09zpRVLoT&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80"
                               , Attrs.alt ""
                               ]
                               []
                           , Htmls.div
                               [ css [ Tw.ml_3 ] ]
                               [ Htmls.div
                                   [ css [ Tw.text_base, Tw.font_medium, Tw.text_white ] ]
                                   [ Htmls.text "Tom Cook" ]
                               , Htmls.div
                                   [ css [ Tw.text_sm, Tw.font_medium, Tw.text_gray_400 ] ]
                                   [ Htmls.text "tom@example.com" ]
                               ]
                           ]
                       ]
                -}
                {- , Htmls.div [ css [ Tw.mt_3, Tw.px_2, Tw.space_y_1 ] ]
                      (menuItems
                          [ Tw.block
                          , Tw.px_3
                          , Tw.py_2
                          , Tw.rounded_md
                          , Tw.text_base
                          , Tw.font_medium
                          , Tw.text_gray_400
                          , Css.hover [ Tw.text_white, Tw.bg_gray_700 ]
                          ]
                      )
                   ]
                -}
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

                {-
                   else if esParaMovil then
                       clasesBase ++ claseExtraPaMovil

                   else
                       clasesBase ++ claseExtraPaDesktop
                -}
                -- ligaChula : List Tw.Styles -> TemplateMetadata.Liga -> Html msg
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

        {- menuItems : List Styles -> List (Html msg)
           menuItems clases =
               [ Htmls.a
                   [ Attrs.href "#"
                   , css clases
                   , Aria.role "menuitem" |> Attrs.fromUnstyled
                   ]
                   [ Htmls.text "Your Profile" ]
               , Htmls.a
                   [ css clases
                   , Aria.role "menuitem" |> Attrs.fromUnstyled
                   ]
                   [ Htmls.text "Settings" ]
               , Htmls.a
                   [ css clases
                   , Aria.role "menuitem" |> Attrs.fromUnstyled
                   ]
                   [ Htmls.text "Sign out" ]
               ]
        -}
        heroiconOutlineMenu : Htmls.Html Msg
        heroiconOutlineMenu =
            Htmls.div
                [ css
                    [ Tw.h_6, Tw.w_6, Tw.block ]
                ]
                [ svg
                    -- [ Svg.Attrsibutes.css [h-6 w-6 block"
                    -- , xmlns="http://www.w3.org/2000/svg"
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
                    -- Svg.Attrsibutes.css [h-6 w-6 block"
                    -- xmlns="http://www.w3.org/2000/svg"
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

                --, myHiddenBlock
                , mobileMenuButton
                ]
            ]
        , myHiddenMenu
        ]
