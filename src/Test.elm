module Test exposing (main)

import Browser
import Dropdown
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input



-- MAIN


type alias Model =
    { sliderValue : Int
    }


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Msg
    = SliderAction Int


initialModel : Model
initialModel =
    { sliderValue = 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SliderAction val ->
            ( { model | sliderValue = val }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


sliderCtl2 : Model -> Element Msg
sliderCtl2 model =
    let
        value =
            model.sliderValue
    in
    Input.slider
        [ Element.height (Element.px 50)
        , Element.width (Element.px 300)
        , Element.inFront
            (Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.centerY
                , Font.center
                ]
                (text (String.fromInt value))
            )
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.centerY
                , Background.color <| rgb255 200 200 200
                ]
                Element.none
            )
        ]
        { onChange = round >> SliderAction
        , label =
            Input.labelLeft [ Element.width (Element.px 50) ]
                (text "slider")
        , min = 0
        , max = toFloat <| 100
        , step = Just 1
        , value = toFloat value
        , thumb =
            Input.thumb
                [ width (Element.px 50)
                , height (Element.px 50)
                , centerY
                , Background.color <| rgb255 34 34 34
                ]
        }


view model =
    layout
        []
    <|
        sliderCtl2 model
