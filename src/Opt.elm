module Opt exposing (..)
import Color

-- tweakable options
opt = {
    trunkClr = Color.hsl 0.3 0.3 0.5,
    trunkH = 80, trunkW1 = 3, trunkW2 = 2, rootH = 0,
    fruitHueScale = 0.1, fruitHueOffs = 1.5, fruitSaturation = 0.4, fruitLightness = 0.7,
    fruitR = 20,
    branchCone = 100, coneDamping = 0.95,
    nestScale = 0.8,
    budTreshold = 17, budSpread = 5, budOffs = 30,
    animDelay = 5, maxNum = 512,
    fontHeight = 22, captionHeight = 27, refH = 300 }
