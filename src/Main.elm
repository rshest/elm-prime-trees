module Main exposing (..)

import Window
import Time
import Html exposing (Html)
import Keyboard

import Opt exposing (..)
import Msg exposing (..)
import Model exposing (..)
import Ptree exposing (..)

subs: Model -> Sub Msg
subs model =
  Sub.batch
    [ Time.every (Time.second*opt.animDelay) Tick
    , Keyboard.presses (\code -> Press code)
    --, Arrows Keyboard.arrows
    , Window.resizes (\{height, width} -> Resize height width)
    ]

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (case msg of
    Arrows {x, y} ->
      { model | num = max 2 ((model.num + x + y)%opt.maxNum) }
    Press 32 ->
      { model | paused = not model.paused }
    Tick _ ->
      { model | num = (model.num +
        (if model.paused then 0 else 1))%opt.maxNum }
    Resize w h ->
      { model | windowWidth = w, windowHeight = h }
    _ -> model
   , Cmd.none)


main =
    Html.program
      {init = (initModel, Cmd.none)
      , update = update
      , view = primeView
      , subscriptions = subs
      }