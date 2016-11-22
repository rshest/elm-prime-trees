module Msg exposing (..)

import Keyboard

type Msg =
    Tick Float
  | Arrows {x: Int, y:Int}
  | Press Keyboard.KeyCode
  | Resize Int Int
