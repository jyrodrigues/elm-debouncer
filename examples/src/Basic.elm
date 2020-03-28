module Basic exposing (Model, Msg(..), init, main, update, view)

{-| This shows an implementation of debouncing your own `Msg` type using
`Debouncer.Basic`. It's actually better to use `Debouncer.Messages` for this --
see the `Messages` example for that simplified approach. But you could use
`Debouncer.Basics` in other cases -- it is more general.
-}

import Browser
import Debouncer.Basic as Debouncer exposing (Debouncer, fromSeconds, provideInput, settleWhenQuietFor, toDebouncer)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Model =
    { debouncer : Debouncer Msg Msg
    , messages : List String
    }

init : ( Model, Cmd Msg )
init =
    ( { debouncer =
            Debouncer.manual
                |> settleWhenQuietFor (Just <| fromSeconds 1)
                |> toDebouncer
      , messages = []
      }
    , Cmd.none
    )

type Msg
    = DebouncerMsg (Debouncer.Msg Msg)
    | DoSomething

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DebouncerMsg subMsg ->
            let
                ( updatedDebouncer, debouncerCmd, emittedMsg ) =
                    Debouncer.update subMsg model.debouncer

                mappedCmd =
                    Cmd.map DebouncerMsg debouncerCmd

                updatedModel =
                    { model | debouncer = updatedDebouncer }
            in
            case emittedMsg of
                Just emitted ->
                    update emitted updatedModel
                        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, mappedCmd ])

                Nothing ->
                    ( updatedModel, mappedCmd )

        DoSomething ->
            ( { model | messages = model.messages ++ [ "I did something" ] }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    div [ style "margin" "1em" ]
        [ button
            [ DoSomething
                |> provideInput
                |> DebouncerMsg
                |> onClick
            ]
            [ text "Click here repeatedly." ]
        , p [] [ text " I'll add a message below once you stop clicking for one second." ]
        , model.messages
            |> List.map (\message -> p [] [ text message ])
            |> div []
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
