module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template)

import Browser.Navigation
import Css
import DataSource
import Footer
import Html as Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Attributes.Aria as Aria
import Html.Events as Event
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
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
            ((myNav model pageView.menu
                |> Html.map toMsg
             )
                :: div
                    [ class "container pl-2 md:pl-6 lg:pl-10" ]
                    pageView.body
                :: [ indexViewFooter ]
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


myNav :
    Model
    -> List View.Liga
    -> Html Msg
myNav modelo getMenu =
    let
        myLogoAndLinks : Html Msg
        myLogoAndLinks =
            div
                [ class "flex items-center" ]
                [ div
                    -- EL LOGO
                    [ class "flex-shrink-0" ]
                    [ Html.img
                        [ class "h-8 w-8"
                        , Attr.src "https://tailwindui.com/img/logos/workflow-mark-indigo-500.svg"
                        , Attr.alt "Workflow"
                        ]
                        []
                    ]
                , div
                    -- LIGAS DE NAVEGACION
                    [ class "hidden, md:block" ]
                    [ div
                        [ class "ml-10 flex items-baseline space-x-4" ]
                        (ligasChulas False getMenu)
                    ]
                ]

        myHiddenMenu : Html Msg
        myHiddenMenu =
            div
                [ class <|
                    "md:hidden "
                        ++ (if modelo.showMobileMenu then
                                "block"

                            else
                                "hidden"
                           )
                ]
                [ div
                    [ class "px-2 pt-2 pb-3 space-y-1 sm:px-3 sm:flex sm:flex-col" ]
                    (ligasChulas True getMenu)
                ]

        ligasChulas : Bool -> List View.Liga -> List (Html Msg)
        ligasChulas showMobileLinks menus =
            let
                clasesQueAplican : Bool -> Bool -> Html.Attribute Msg
                clasesQueAplican muestraMovil esResaltado =
                    case ( muestraMovil, esResaltado ) of
                        ( True, True ) ->
                            class "bg-gray-900 text-white block px-3 py-2 rounded-md text-base font-medium"

                        ( True, False ) ->
                            class "text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium"

                        ( False, True ) ->
                            class "bg-gray-900 text-white px-3 py-2 rounded-md text-base font-medium"

                        ( False, False ) ->
                            class "text-amber-200 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-base font-medium"

                ligaChula clase liga =
                    Route.link
                        liga.direccion
                        [ clase ]
                        [ text liga.queDice ]
            in
            List.map
                (\unaLiga ->
                    ligaChula
                        (clasesQueAplican showMobileLinks False)
                        unaLiga
                )
                menus

        heroiconOutlineMenu : Html.Html Msg
        heroiconOutlineMenu =
            div
                [ class "h-6 w-6 block" ]
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

        heroiconOutlineX : Html Msg
        heroiconOutlineX =
            div
                [ class "h-6 w-6 block" ]
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

        mobileMenuButton : Html Msg
        mobileMenuButton =
            div
                [ class "-mr-2 flex md:hidden" ]
                [ Html.button
                    [ class "bg-gray-800 inline-flex items-center justify-center p-2 rounded-md text-gray-400 hover:text-white hover:bg-gray-700 focus:outline-none focus:ring-2 focus focus:ring-offset-2 focus:ring-offset-gray-800 focus:ring-white"
                    , Event.onClick ToggleMobileMenu
                    ]
                    [ Html.span
                        [ class "sr-only" ]
                        [ text "Open main menu" ]
                    , Html.span
                        []
                        [ if modelo.showMobileMenu then
                            heroiconOutlineX

                          else
                            heroiconOutlineMenu
                        ]
                    ]
                ]
    in
    Html.nav
        [ class "bg-gray-800" ]
        [ div
            [ class "max-w-7xl mx-auto px-4 lg:px-8 sm:px-6" ]
            [ div
                [ class "flex items-center justify-between h-16" ]
                [ myLogoAndLinks
                , mobileMenuButton
                ]
            ]
        , myHiddenMenu
        ]
