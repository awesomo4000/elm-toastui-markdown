port module Main exposing (main)

import Browser
import Hex
import Html exposing (Html, button, code, div, span, text)
import Html.Attributes
    exposing
        ( checked
        , disabled
        , style
        , type_
        )
import Html.Events exposing (onClick)
import Murmur3
import Process
import Task
import ToastUiEditor as Toast


port log : String -> Cmd msg


type Model
    = NoEditor
    | Data String



{--INIT --}


init : () -> ( Model, Cmd Msg )
init _ =
    ( Data "", log "Loaded" )



{--VIEW --}


view : Model -> Browser.Document Msg
view model =
    { title = "Toast UI Editor"
    , body = [ view_ model ]
    }


view_ : Model -> Html Msg
view_ model =
    case model of
        NoEditor ->
            div []
                [ bar
                    [ model |> statusDiv, loadButtons ]
                ]

        Data s ->
            div []
                [ bar
                    [ model |> statusDiv
                    , loadButtons
                    ]
                , toastEditor s
                ]


loadButtons =
    div []
        [ loadButton "LOAD DOC" (Data doc), loadButton "BLANK" (Data "") ]


loadButton : String -> Model -> Html Msg
loadButton label m =
    button [ onClick <| ClickedLoad m ] [ text label ]


bar : List (Html Msg) -> Html Msg
bar elements =
    let
        container =
            div
                [ style "display" "flex"
                , style "padding" "4px"
                , style "justify-content" "space-between"
                , style "margin-bottom" "4px"
                , style "border-bottom" "1px"
                , style "border-bottom-style" "solid"
                , style "border-bottom-color" "lightgray"
                ]
    in
    container elements


modelToStatusString : Model -> String
modelToStatusString model =
    case model of
        NoEditor ->
            "...Loading..."

        Data s ->
            s |> hexHash


statusDiv : Model -> Html msg
statusDiv model =
    div []
        [ code
            [ style "color" "#017575"
            , style "font-family" "Monaco"
            , style "font-weight" ""
            , style "font-size" "12px"
            , style "border-radius" "4px"
            , style "margin-left" "16px"
            ]
            [ text (modelToStatusString model) ]
        ]



{--UPDATE --}


type alias Delay =
    Float


type Msg
    = Batch (List Msg)
    | BatchDelayed Delay (List Msg)
    | Log String
    | SetModel Model
    | ClickedLoad Model
    | OnEditorChange Toast.Content


sendMsg : msg -> Cmd msg
sendMsg msg =
    Task.succeed msg |> Task.perform identity


sendMsgDelay : Delay -> msg -> Cmd msg
sendMsgDelay delay msg =
    Task.perform (\_ -> msg) (Process.sleep delay)


batchDelayed : Float -> List Msg -> Cmd Msg
batchDelayed delay msgs_ =
    sendMsgDelay delay <| BatchDelayed delay msgs_


delayTime =
    42


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Batch [] ->
            ( model, Cmd.none )

        Batch (x :: xs) ->
            let
                ( newModel, cmd ) =
                    update x model
            in
            ( newModel
            , Cmd.batch [ cmd, sendMsg (Batch xs) ]
            )

        BatchDelayed t [] ->
            ( model, Cmd.none )

        BatchDelayed t (x :: xs) ->
            let
                ( newModel, cmd ) =
                    update x model
            in
            ( newModel
            , Cmd.batch
                [ cmd, sendMsgDelay t (BatchDelayed t xs) ]
            )

        Log str ->
            ( model, log str )

        ClickedLoad model_ ->
            ( NoEditor
            , batchDelayed delayTime [ SetModel model_ ]
            )

        OnEditorChange change ->
            ( Data change, Cmd.none )

        SetModel m ->
            ( m, Cmd.none )



{--MAIN --}


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


defaultEditorOpts =
    Toast.defaults
        |> Toast.withTheme Toast.Light
        |> Toast.withMode Toast.Wysiwyg
        |> Toast.withHeight "calc(100vh - 8em)"
        |> Toast.withZoom "1.25"


toastEditorDestroy : Toast.Content -> Html Msg
toastEditorDestroy content =
    Toast.view
        (defaultEditorOpts
            |> Toast.withInitialValue content
            |> Toast.destroy True
            |> Toast.onChange (Just OnEditorChange)
        )


toastEditor : Toast.Content -> Html Msg
toastEditor content =
    Toast.view
        (defaultEditorOpts
            |> Toast.withInitialValue content
            |> Toast.onChange (Just OnEditorChange)
        )


nbsp : String
nbsp =
    "\u{00A0}"


hexHash : Toast.Content -> String
hexHash str =
    str
        |> Murmur3.hashString 1234
        |> Hex.toString
        |> (++) "0x"



-- viewCheckbox : Bool -> Bool -> msg -> String -> Html msg
-- viewCheckbox isDisabled isChecked msg description =
--     label labelStyle
--         [ span [ style "padding" "1rem" ] [ text description ]
--         , input
--             [ type_ "checkbox"
--             , checked isChecked
--             , disabled isDisabled
--             , onClick msg
--             , style "zoom" "1.3"
--             ]
--             []
--         ]
-- labelStyle : List (Html.Attribute msg)
-- labelStyle =
--     [ style "padding" "0.25em"
--     , style "border-style" "solid"
--     , style "border-width" "1px"
--     , style "border-color" "#efefef"
--     , style "-webkit-user-select" "none"
--     ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



{--test doc --}


doc : String
doc =
    """
# heading 1

some text

## heading 2

* a **list**
* **another** item
* *third*

> quote
> me
> on

```python
for x in range(100):
    print(x)

```"""
