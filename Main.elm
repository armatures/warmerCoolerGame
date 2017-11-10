module Main exposing (..)

import Html exposing (Html, div, text, program)
import Html.Attributes exposing (style)
import Mouse
import Keyboard
import Plot
import Svg exposing (..)
import Svg.Attributes exposing (..)


-- MODEL


type alias Model =
    { counter: Int
    , clicks : List Mouse.Position
    , height : Int
    , width : Int
    }


init : ( Model, Cmd Msg )
init =
    ( {counter = 0
      , clicks = []
      , height = 600
      , width = 600
      }
    , Cmd.none )



-- MESSAGES


type Msg
    = MouseMsg Mouse.Position
    | KeyMsg Keyboard.KeyCode



-- VIEW


view : Model -> Html Msg
view model =
   div []
         [ svg
          [ version "1.1", baseProfile "full", width (toString model.width), height (toString model.height)]
          ([ rect [width "100%", height "100%", fill "green"] []
          ] ++ (showClicks model.clicks))
         , div [] <| List.map showClick model.clicks
         ]
showClicks =
    let show point =
          circle [cx (toString point.x), cy (toString point.y), r "40", fill "white"][]
    in List.map show
showClick : a -> Html msg
showClick =
    (Html.text << toString)


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let updateClicks pos =
            if pos.x < model.width && pos.y < model.height then
                pos :: model.clicks
            else
                []
    in
        case msg of
            MouseMsg position ->
                ( { model | counter = model.counter + 1
                  , clicks = updateClicks position
                  }, Cmd.none )

            KeyMsg code ->
                ( {model | counter = model.counter + 2}, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseMsg
        , Keyboard.downs KeyMsg
        ]


-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
