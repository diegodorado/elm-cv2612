module Main exposing (main)

import Browser
import Dropdown
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Icon as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html
import Html.Attributes
import Http



-- MAIN


type alias ScenePatch =
    { scene : Float
    , a : Patch
    , b : Patch
    , c : Patch
    , d : Patch
    }


type PatchParam
    = LowFrequencyOscillator


type PatchNumber
    = PatchA
    | PatchB
    | PatchC
    | PatchD


type alias Patch =
    { lfo : Int
    , ch1 : Channel
    , ch2 : Channel
    , ch3 : Channel
    , ch4 : Channel
    , ch5 : Channel
    , ch6 : Channel
    }


type ChannelParam
    = Feedback
    | AmplitudeModulationSensitivity
    | FrequencyModulationSensitivity
    | Algorithm
    | Stereo


type ChannelNumber
    = ChannelOne
    | ChannelTwo
    | ChannelThree
    | ChannelFour
    | ChannelFive
    | ChannelSix


type alias Channel =
    { fb : Int
    , st : Int
    , al : Int
    , ams : Int
    , fms : Int
    , op1 : Operator
    , op2 : Operator
    , op3 : Operator
    , op4 : Operator
    }


type alias Operator =
    { ar : Int
    , d1 : Int
    , sl : Int
    , d2 : Int
    , rr : Int
    , tl : Int
    , ml : Int
    , dt : Int
    , rs : Int
    , am : Int
    }


type OperatorParam
    = AttackRate
    | Decay1Rate
    | SustainLevel
    | Decay2Rate
    | ReleaseRate
    | TotalLevel
    | Multiplier
    | Detune
    | RateScale
    | AmplitudeModulation


type OperatorNumber
    = OperatorOne
    | OperatorTwo
    | OperatorThree
    | OperatorFour


type Param
    = OperatorParam OperatorNumber OperatorParam
    | ChannelParam ChannelParam
    | PatchParam PatchParam


type alias Instrument =
    { label : String
    }


type alias Model =
    { instrumentsState : InstrumentsState
    , instruments : List Instrument
    , scenePatch : ScenePatch
    , patchNumber : PatchNumber
    , channelNumber : ChannelNumber
    , sliderValue : Int
    , selectedInstrumentId : Maybe String
    , dropdownState : Dropdown.State
    }


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type InstrumentsState
    = Failure
    | Loading
    | Success String


type ParamMsg
    = AdjustPatchParam PatchParam Int
    | AdjustChannelParam ChannelParam Int
    | AdjustOperatorParam OperatorNumber OperatorParam Int


type Msg
    = ParamMsg ParamMsg
    | SliderAction Int
    | GotText (Result Http.Error String)
    | NoOp
    | OnSelect (Maybe Instrument)
    | DropdownMsg (Dropdown.Msg Instrument)
    | PreviousPatchClick
    | NextPatchClick
    | SavePatchClick
    | CreatePatchClick
    | UploadPatchClick
    | DeletePatchClick
    | ClearPatchesClick


theme =
    { blue = rgb255 80 158 236
    , black = rgb255 34 34 34
    , white = rgb255 221 221 221
    }


initialModel : Model
initialModel =
    let
        op =
            Operator 0 0 0 0 0 0 0 0 0 0

        ch =
            Channel 0 0 0 0 0 op op op op

        patch =
            Patch 0 ch ch ch ch ch ch

        scenePatch =
            ScenePatch 0 patch patch patch patch
    in
    { instrumentsState = Loading
    , instruments = []
    , scenePatch = scenePatch
    , patchNumber = PatchA
    , sliderValue = 0
    , channelNumber = ChannelOne
    , selectedInstrumentId = Nothing
    , dropdownState = Dropdown.newState ""
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = "/assets/cv2612/instruments/index.txt"
        , expect = Http.expectString GotText
        }
    )



-- UPDATE


dropdownConfig : Dropdown.Config Msg Instrument
dropdownConfig =
    Dropdown.newConfig OnSelect .label
        |> Dropdown.withSelectedStyles [ ( "color", "grey" ) ]
        |> Dropdown.withClear False
        |> Dropdown.withMenuStyles [ ( "height", "300px" ), ( "z-index", "100" ), ( "background", "#222" ), ( "overflow-y", "scroll" ) ]
        |> Dropdown.withPrompt "Select Instrument Preset"
        |> Dropdown.withTriggerStyles [ ( "width", "100%" ), ( "height", "100%" ), ( "outline", "0" ) ]


getPatch : PatchNumber -> ScenePatch -> Patch
getPatch patchNumber scenePatch =
    case patchNumber of
        PatchA ->
            scenePatch.a

        PatchB ->
            scenePatch.b

        PatchC ->
            scenePatch.c

        PatchD ->
            scenePatch.d


getChannel : ChannelNumber -> Patch -> Channel
getChannel chNumber patch =
    case chNumber of
        ChannelOne ->
            patch.ch1

        ChannelTwo ->
            patch.ch2

        ChannelThree ->
            patch.ch3

        ChannelFour ->
            patch.ch4

        ChannelFive ->
            patch.ch5

        ChannelSix ->
            patch.ch6


getOperator : OperatorNumber -> Channel -> Operator
getOperator opNumber channel =
    case opNumber of
        OperatorOne ->
            channel.op1

        OperatorTwo ->
            channel.op2

        OperatorThree ->
            channel.op3

        OperatorFour ->
            channel.op4


updatePatch : Patch -> PatchNumber -> ScenePatch -> ScenePatch
updatePatch patch patchNumber oldScenePatch =
    case patchNumber of
        PatchA ->
            { oldScenePatch | a = patch }

        PatchB ->
            { oldScenePatch | b = patch }

        PatchC ->
            { oldScenePatch | c = patch }

        PatchD ->
            { oldScenePatch | d = patch }


updateChannel : Channel -> PatchNumber -> ChannelNumber -> Patch -> ScenePatch -> ScenePatch
updateChannel channel patchNumber chNumber oldPatch oldScenePatch =
    let
        patch =
            case chNumber of
                ChannelOne ->
                    { oldPatch | ch1 = channel }

                ChannelTwo ->
                    { oldPatch | ch2 = channel }

                ChannelThree ->
                    { oldPatch | ch3 = channel }

                ChannelFour ->
                    { oldPatch | ch4 = channel }

                ChannelFive ->
                    { oldPatch | ch5 = channel }

                ChannelSix ->
                    { oldPatch | ch6 = channel }
    in
    updatePatch patch patchNumber oldScenePatch


updateOperator : Operator -> PatchNumber -> ChannelNumber -> OperatorNumber -> Channel -> Patch -> ScenePatch -> ScenePatch
updateOperator operator patchNumber chNumber opNumber oldChannel oldPatch oldScenePatch =
    let
        channel =
            case opNumber of
                OperatorOne ->
                    { oldChannel | op1 = operator }

                OperatorTwo ->
                    { oldChannel | op2 = operator }

                OperatorThree ->
                    { oldChannel | op3 = operator }

                OperatorFour ->
                    { oldChannel | op4 = operator }
    in
    updateChannel channel patchNumber chNumber oldPatch oldScenePatch


updateParam : ParamMsg -> Model -> ( Model, Cmd Msg )
updateParam paramMsg model =
    let
        scenePatch =
            model.scenePatch

        patch =
            getPatch model.patchNumber scenePatch

        patchNumber =
            model.patchNumber

        channel =
            getChannel model.channelNumber patch

        chNumber =
            model.channelNumber
    in
    case paramMsg of
        AdjustPatchParam patchParam value ->
            case patchParam of
                LowFrequencyOscillator ->
                    ( { model | scenePatch = updatePatch { patch | lfo = value } patchNumber scenePatch }, Cmd.none )

        AdjustChannelParam chParam value ->
            case chParam of
                Feedback ->
                    ( { model | scenePatch = updateChannel { channel | fb = value } patchNumber chNumber patch scenePatch }, Cmd.none )

                Algorithm ->
                    ( { model | scenePatch = updateChannel { channel | al = value } patchNumber chNumber patch scenePatch }, Cmd.none )

                Stereo ->
                    ( { model | scenePatch = updateChannel { channel | st = value } patchNumber chNumber patch scenePatch }, Cmd.none )

                AmplitudeModulationSensitivity ->
                    ( { model | scenePatch = updateChannel { channel | ams = value } patchNumber chNumber patch scenePatch }, Cmd.none )

                FrequencyModulationSensitivity ->
                    ( { model | scenePatch = updateChannel { channel | fms = value } patchNumber chNumber patch scenePatch }, Cmd.none )

        AdjustOperatorParam opNumber opParam value ->
            let
                operator =
                    getOperator opNumber channel
            in
            case opParam of
                AttackRate ->
                    ( { model | scenePatch = updateOperator { operator | ar = value } patchNumber chNumber opNumber channel patch scenePatch }, Cmd.none )

                Decay1Rate ->
                    ( { model | scenePatch = updateOperator { operator | d1 = value } patchNumber chNumber opNumber channel patch scenePatch }, Cmd.none )

                SustainLevel ->
                    ( { model | scenePatch = updateOperator { operator | sl = value } patchNumber chNumber opNumber channel patch scenePatch }, Cmd.none )

                Decay2Rate ->
                    ( { model | scenePatch = updateOperator { operator | d2 = value } patchNumber chNumber opNumber channel patch scenePatch }, Cmd.none )

                ReleaseRate ->
                    ( { model | scenePatch = updateOperator { operator | rr = value } patchNumber chNumber opNumber channel patch scenePatch }, Cmd.none )

                TotalLevel ->
                    ( { model | scenePatch = updateOperator { operator | tl = value } patchNumber chNumber opNumber channel patch scenePatch }, Cmd.none )

                Multiplier ->
                    ( { model | scenePatch = updateOperator { operator | ml = value } patchNumber chNumber opNumber channel patch scenePatch }, Cmd.none )

                Detune ->
                    ( { model | scenePatch = updateOperator { operator | dt = value } patchNumber chNumber opNumber channel patch scenePatch }, Cmd.none )

                RateScale ->
                    ( { model | scenePatch = updateOperator { operator | rs = value } patchNumber chNumber opNumber channel patch scenePatch }, Cmd.none )

                AmplitudeModulation ->
                    ( { model | scenePatch = updateOperator { operator | am = value } patchNumber chNumber opNumber channel patch scenePatch }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ParamMsg paramMsg ->
            updateParam paramMsg model

        SliderAction val ->
            ( { model | sliderValue = val }, Cmd.none )

        GotText result ->
            let
                instruments =
                    case result of
                        Ok fullText ->
                            String.lines fullText
                                |> List.filter (\l -> not <| String.isEmpty l)
                                |> List.map (\l -> Instrument l)

                        Err _ ->
                            [ Instrument "Failure..." ]
            in
            ( { model | instruments = instruments }, Cmd.none )

        OnSelect maybeInstrument ->
            let
                maybeId =
                    maybeInstrument |> Maybe.map .label
            in
            ( { model | selectedInstrumentId = maybeId }, Cmd.none )

        -- Route message to the Dropdown component.
        -- The returned command is important.
        DropdownMsg subMsg ->
            let
                ( updated, cmd ) =
                    Dropdown.update dropdownConfig subMsg model.dropdownState
            in
            ( { model | dropdownState = updated }, cmd )

        NoOp ->
            ( model, Cmd.none )

        PreviousPatchClick ->
            ( model, Cmd.none )

        NextPatchClick ->
            ( model, Cmd.none )

        SavePatchClick ->
            ( model, Cmd.none )

        CreatePatchClick ->
            ( model, Cmd.none )

        UploadPatchClick ->
            ( model, Cmd.none )

        DeletePatchClick ->
            ( model, Cmd.none )

        ClearPatchesClick ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


paramsBitness : Param -> Int
paramsBitness param =
    case param of
        PatchParam patchParam ->
            case patchParam of
                LowFrequencyOscillator ->
                    3

        ChannelParam chParam ->
            case chParam of
                Feedback ->
                    3

                AmplitudeModulationSensitivity ->
                    3

                FrequencyModulationSensitivity ->
                    3

                Algorithm ->
                    3

                Stereo ->
                    2

        OperatorParam _ opParam ->
            case opParam of
                AttackRate ->
                    5

                Decay1Rate ->
                    5

                SustainLevel ->
                    4

                Decay2Rate ->
                    5

                ReleaseRate ->
                    4

                TotalLevel ->
                    7

                Multiplier ->
                    4

                Detune ->
                    3

                RateScale ->
                    3

                AmplitudeModulation ->
                    1


sliderCtlMax : Param -> Int
sliderCtlMax param =
    2 ^ paramsBitness param - 1


sliderCtlName : Param -> String
sliderCtlName param =
    case param of
        PatchParam patchParam ->
            case patchParam of
                LowFrequencyOscillator ->
                    "lfo"

        ChannelParam chParam ->
            case chParam of
                Feedback ->
                    "fb"

                AmplitudeModulationSensitivity ->
                    "ams"

                FrequencyModulationSensitivity ->
                    "fms"

                Algorithm ->
                    "al"

                Stereo ->
                    "st"

        OperatorParam _ opParam ->
            case opParam of
                AttackRate ->
                    "ar"

                Decay1Rate ->
                    "d1"

                SustainLevel ->
                    "sl"

                Decay2Rate ->
                    "d2"

                ReleaseRate ->
                    "rr"

                TotalLevel ->
                    "tl"

                Multiplier ->
                    "ml"

                Detune ->
                    "dt"

                RateScale ->
                    "rs"

                AmplitudeModulation ->
                    "am"


sliderCtlValue : Param -> Model -> Int
sliderCtlValue param model =
    let
        patch =
            getPatch model.patchNumber model.scenePatch

        channel =
            getChannel model.channelNumber patch
    in
    case param of
        PatchParam patchParam ->
            case patchParam of
                LowFrequencyOscillator ->
                    patch.lfo

        ChannelParam chParam ->
            case chParam of
                Feedback ->
                    channel.fb

                AmplitudeModulationSensitivity ->
                    channel.ams

                FrequencyModulationSensitivity ->
                    channel.fms

                Algorithm ->
                    channel.al

                Stereo ->
                    channel.st

        OperatorParam opNumber opParam ->
            let
                operator =
                    getOperator opNumber channel
            in
            case opParam of
                AttackRate ->
                    operator.ar

                Decay1Rate ->
                    operator.d1

                SustainLevel ->
                    operator.sl

                Decay2Rate ->
                    operator.d2

                ReleaseRate ->
                    operator.rr

                TotalLevel ->
                    operator.tl

                Multiplier ->
                    operator.ml

                Detune ->
                    operator.dt

                RateScale ->
                    operator.rs

                AmplitudeModulation ->
                    operator.am


sliderCtlAction : Param -> Int -> Msg
sliderCtlAction param =
    case param of
        PatchParam patchParam ->
            ParamMsg << AdjustPatchParam patchParam

        ChannelParam chParam ->
            ParamMsg << AdjustChannelParam chParam

        OperatorParam opNumber opParam ->
            ParamMsg << AdjustOperatorParam opNumber opParam


sliderCtl : Param -> Model -> Element Msg
sliderCtl param model =
    let
        name =
            sliderCtlName param

        value =
            sliderCtlValue param model
    in
    Input.slider
        [ Element.height (Element.px 20)
        , Element.width (Element.px 200)
        , Element.inFront
            (Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.centerY
                , Font.center
                , Font.color theme.black
                ]
                (text (String.fromInt value))
            )
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.centerY
                , Background.color <| theme.white
                ]
                Element.none
            )
        ]
        { onChange = round >> sliderCtlAction param
        , label =
            Input.labelLeft [ Element.width (Element.px 50) ]
                (text name)
        , min = 0
        , max = toFloat <| sliderCtlMax param
        , step = Just 1
        , value = toFloat value
        , thumb =
            Input.thumb
                [ Element.width (Element.px 20)
                , Element.height (Element.px 20)
                , Element.centerY
                , Background.color <| theme.blue
                ]
        }


sliderCtl2 : Model -> Element Msg
sliderCtl2 model =
    let
        name =
            "ee"

        value =
            model.sliderValue
    in
    Input.slider
        [ Element.height (Element.px 20)
        , Element.width (Element.px 200)
        , Element.inFront
            (Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.centerY
                , Font.center
                , Font.color theme.black
                ]
                (text (String.fromInt value))
            )
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.centerY
                , Background.color <| theme.white
                ]
                Element.none
            )
        ]
        { onChange = round >> SliderAction
        , label =
            Input.labelLeft [ Element.width (Element.px 50) ]
                (text name)
        , min = 0
        , max = toFloat <| 100
        , step = Just 1
        , value = toFloat value
        , thumb =
            Input.thumb
                [ Element.width (Element.px 20)
                , Element.height (Element.px 20)
                , Element.centerY
                , Background.color <| theme.blue
                ]
        }


renderList : List String -> Element Msg
renderList lst =
    lst
        |> List.map (\l -> el [] (text l))
        |> Element.column []


menuItem : Icon.Icon -> Msg -> Element Msg
menuItem icon action =
    el
        [ centerY
        , height fill
        , padding 10
        , width fill
        , mouseOver
            [ Background.color theme.white
            , Font.color theme.black
            ]
        , htmlAttribute <| Html.Attributes.style "cursor" "pointer"
        , onClick action
        ]
    <|
        html <|
            Icon.viewIcon icon


patchesDropdown : Model -> Element Msg
patchesDropdown model =
    let
        selectedInstrument =
            case model.selectedInstrumentId of
                Nothing ->
                    Nothing

                Just name ->
                    List.filter (\m -> m.label == name) model.instruments
                        |> List.head
    in
    el [ width (fillPortion 5), height fill, padding 10 ] <|
        html <|
            Html.map DropdownMsg (Dropdown.view dropdownConfig model.dropdownState model.instruments selectedInstrument)


patchesMenu : Model -> Element Msg
patchesMenu model =
    row [ height (px 50), width fill, spaceEvenly, Border.width 1, Border.color theme.white ]
        [ menuItem Icon.caretLeft PreviousPatchClick
        , patchesDropdown model
        , menuItem Icon.caretRight NextPatchClick
        , menuItem Icon.save SavePatchClick
        , menuItem Icon.plus CreatePatchClick
        , menuItem Icon.upload UploadPatchClick
        , menuItem Icon.backspace DeletePatchClick
        , menuItem Icon.bomb ClearPatchesClick
        ]


operatorView : OperatorNumber -> Model -> Element Msg
operatorView opNumber model =
    column [ spacing 8 ]
        [ sliderCtl (OperatorParam opNumber AttackRate) model
        , sliderCtl (OperatorParam opNumber Decay1Rate) model
        , sliderCtl (OperatorParam opNumber SustainLevel) model
        , sliderCtl (OperatorParam opNumber Decay2Rate) model
        , sliderCtl (OperatorParam opNumber ReleaseRate) model
        , sliderCtl (OperatorParam opNumber TotalLevel) model

        -- ENVELOPE
        , el [ width fill, Font.center ] <| text "ENVELOPE"
        , sliderCtl (OperatorParam opNumber Multiplier) model
        , sliderCtl (OperatorParam opNumber Detune) model
        , sliderCtl (OperatorParam opNumber RateScale) model
        , sliderCtl (OperatorParam opNumber AmplitudeModulation) model
        ]


view model =
    layout
        [ height fill
        , width fill
        , padding 20
        , Background.color <| theme.black
        , Font.color theme.white
        , Font.size 20
        , Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        ]
    <|
        column [ width (fill |> maximum 1280), centerX, spacing 40 ]
            [ patchesMenu model
            , sliderCtl (PatchParam LowFrequencyOscillator) model

            -- CHANNEL PARAMS
            , row [ width fill ]
                [ column [ width (fillPortion 3), alignTop ]
                    [ wrappedRow [ width fill, spaceEvenly, spacing 8 ]
                        [ sliderCtl (ChannelParam Feedback) model
                        , sliderCtl (ChannelParam Stereo) model
                        , sliderCtl (ChannelParam Algorithm) model
                        , sliderCtl (ChannelParam AmplitudeModulationSensitivity) model
                        , sliderCtl (ChannelParam FrequencyModulationSensitivity) model
                        ]
                    ]
                , column [ width (fillPortion 1), alignTop ]
                    [ el [ width fill, Font.center ] <| text "ALGORITHM"
                    ]
                ]

            -- OPERATORS
            , row [ width fill, spaceEvenly ]
                [ operatorView OperatorOne model
                , operatorView OperatorTwo model
                , operatorView OperatorThree model
                , operatorView OperatorFour model
                ]
            ]
