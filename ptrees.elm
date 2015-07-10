module Ptree where

import Color 
import Time
import List 
import Window
import Text
import String

import Signal exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

goldenPhi: Float
goldenPhi = 137.5/180.0*pi

opt = { trunkClr = Color.hsl 0.3 0.5 0.4, 
        trunkH = 80, trunkW1 = 3, trunkW2 = 2, rootH = 0, 
        fruitHue = 1.5, fruitR = 20,
        branchCone = 100, coneDamping = 0.95, 
        nestScale = 0.8, 
        budTreshold = 11, budSpread = 5, budOffs = 20, 
        animDelay = 5, maxNum = 512 }

trunk: Form
trunk = polygon [(-opt.trunkW1, 0),
                 (-opt.trunkW2, opt.trunkH),
                 (opt.trunkW2, opt.trunkH), 
                 (opt.trunkW1, 0),
                 (0, -opt.rootH)]
      |> filled opt.trunkClr
         
fruit: Float -> Form
fruit h = circle opt.fruitR
        |> gradient (fruitGrad h)

branchAngle: Float -> Float -> Float -> Float
branchAngle cone i n =
  if n <= 1 then 0
  else (degrees (i - (n - 1)/2)*cone/(n - 1))

branchFork: Int -> Float -> List Int -> Int -> Form
branchFork n cone xs i = 
    if n <= opt.budTreshold then
        -- a branch fork
        [trunk, 
         subTree cone xs 
         |> moveY opt.trunkH 
         |> scale opt.nestScale] 
        |> group
        |> rotate (branchAngle cone (toFloat i) (toFloat n))
    else 
        -- a bud
        let ang = goldenPhi*(toFloat i)
            r = sqrt(ang)*opt.budSpread
        in [subTree cone xs 
            |> moveY r 
            |> scale (opt.nestScale*0.5)] 
           |> group 
           |> rotate ang 
           |> moveY opt.budOffs      

subTree: Float -> List Int -> Form
subTree cone s = 
  case s of
    n::xs -> [0..(n - 1)] 
     |> List.map (branchFork n (cone*opt.coneDamping) xs) 
     |> group
    _ -> fruit opt.fruitHue


ptree: List Int -> Element         
ptree factors = collage 400 300 
                [subTree opt.branchCone factors 
                     |> moveY -150]

factorsString: List Int -> String
factorsString factors = 
    factors 
        |> List.drop 1
        |> List.map toString
        |> String.join "x"

caption: Int -> List Int -> Element
caption n factors = 
    toString n ++  
         (if List.length factors == 2 
          then " - PRIME!" 
          else " = " ++ factorsString factors)
        |> Text.fromString 
        |> Text.bold
        |> Text.color Color.blue
        |> Text.height 20
        |> centered

primeView: Int -> Element
primeView n = 
    let factors = primes n in           
    flow down [caption n factors |> width 400, ptree factors]

main: Signal Element
main = 
    primeView 
    << (\n -> (n//opt.animDelay)%(opt.maxNum) + 1) 
    << round 
    << Time.inSeconds 
    <~ Time.every Time.second

fruitGrad: Float -> Color.Gradient
fruitGrad h = 
    Color.radial (0, 0) 3 
           (4, -4) 16
           [(0, Color.hsl h 0.5 0.8), 
            (1, Color.hsl h 0.5 0.5)]

primeFactors: Int -> Int -> List Int
primeFactors n s = 
  if | s*s > n -> [n]
     | n%s == 0 -> s::(primeFactors (n//s) s)
     | s == 2 -> primeFactors n (s + 1)
     | otherwise -> primeFactors n (s + 2)

primes: Int -> List Int
primes n = 1::(primeFactors n 2)

