module Footer exposing (..)

import HeroIcons
import Html as Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Events as Event


ligaAlPie : String -> String -> Html msg
ligaAlPie liga texto =
    div
        [ class "px-5 py-2" ]
        [ Html.a
            [ Attr.href liga
            , class " text-base text-gray-600 hover:text-black"
            ]
            [ text texto ]
        ]


ligaIcono : String -> String -> SocialIcons -> Html msg
ligaIcono direccion srCual iconoSocial =
    Html.a
        [ Attr.href direccion
        , class "text-gray-400 hover:text-gray-500"
        ]
        [ Html.span
            [ class "sr-only" ]
            [ text srCual ]
        , case iconoSocial of
            Facebook ->
                HeroIcons.svgFacebook

            Instagram ->
                HeroIcons.svgInstagram

            Twitter ->
                HeroIcons.svgTwitter

            Github ->
                HeroIcons.svgGithub

            LinkedIn ->
                HeroIcons.svgLinkedIn

            Email ->
                HeroIcons.svgMailIcon

            WhatsApp ->
                HeroIcons.svgWhatsApp
        ]


{-| De la página donde se llame la función viewFooter
Hay que definir dos cosas:

primero:
viewPieNavega : List (Html msg)

usando la función:
Footer.ligaAlPie "#" "About"

y segundo:
viewPieSocialIcons : List (Htmls.Html msg)

usando la función:
Footer.ligaIcono "facebook.com" "facebook" Footer.Facebook

Finalmente llamar esta función estableciendo también el texto tipo String
que va en el footer y será la variable copyR.

Así el footer queda desacoplado de sus ligas e íconos que se establecen en cada página.

-}
viewFooter : List (Html msg) -> List (Html msg) -> String -> Html msg
viewFooter ligasNav icons2show copyR =
    Html.footer
        []
        [ div
            [ class "bg-white" ]
            -- "py-4"
            []
        , div
            [ class "bg-gray-200" ]
            [ div
                [ class "max-w-7xl mx-auto py-12 px-4 overflow-hidden lg:px-8 sm:px-6" ]
                [ Html.nav
                    [ class "-mx-5 -my-2 flex flex-wrap justify-center"
                    , Attr.attribute "aria-label" "Footer"
                    ]
                    ligasNav
                , div
                    [ class "mt-8 flex justify-center space-x-6" ]
                    icons2show
                , Html.p
                    [ class "mt-8 text-center text-base text-gray-500" ]
                    [ text copyR ]
                ]
            ]
        ]


type SocialIcons
    = Facebook
    | Instagram
    | Twitter
    | Github
    | LinkedIn
    | WhatsApp
    | Email
