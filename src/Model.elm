module Model exposing (..)

type alias Model =
  { num : Int -- current number
  , paused : Bool -- whether currently paused
  , windowWidth : Int
  , windowHeight : Int
  }

initModel : Model
initModel =
  { num = 2
  , paused = False
  , windowWidth = 640
  , windowHeight = 480
  }