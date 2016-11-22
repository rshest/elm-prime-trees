module Ptree exposing (..)

import List
import String
import Color 
import Text
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Opt exposing (..)

goldenPhi: Float
goldenPhi = 137.5/180.0*pi


primeFactors: Int -> Int -> List Int
primeFactors n s = 
  if s*s > n then [n]
  else if n%s == 0 then s::(primeFactors (n//s) s)
  else if s == 2 then primeFactors n (s + 1)
  else primeFactors n (s + 2)

                    
primes: Int -> List Int
primes n = 1::(primeFactors n 2)

fruitGrad: Float -> Color.Gradient
fruitGrad h = 
    Color.radial (0, 0) 5 (4, -4) 16
             [(0, Color.hsl h opt.fruitSaturation (opt.fruitLightness + 0.2)), 
            (1, Color.hsl h opt.fruitSaturation opt.fruitLightness)]

trunk: Form
trunk = 
    polygon [(-opt.trunkW1, 0),
             (-opt.trunkW2, opt.trunkH),
             (opt.trunkW2, opt.trunkH), 
             (opt.trunkW1, 0),
             (0, -opt.rootH)]
        |> filled opt.trunkClr

         
fruit: Float -> Form
fruit h = 
    circle opt.fruitR
        |> gradient (fruitGrad h)
           

branchFork: Int -> Float -> List Int -> Int -> Form
branchFork n cone xs i = 
    if n <= opt.budTreshold then
        -- a branch fork
        let fn = toFloat n
            fi = toFloat i
            branchAngle = 
                if n <= 1 then 0 else
                    degrees (fi - (fn - 1)/2)*cone/(fn - 1)
        in
          [trunk, 
           subTree cone xs i
               |> moveY opt.trunkH 
               |> scale opt.nestScale] 
          |> group
          |> rotate branchAngle
    else 
        -- a bud
        let ang = goldenPhi*(toFloat i)
            r = sqrt(ang)*opt.budSpread
        in 
          [subTree cone xs i 
              |> moveY r 
              |> scale (opt.nestScale*0.5)] 
          |> group 
          |> rotate ang 
          |> moveY opt.budOffs      

             
subTree: Float -> List Int -> Int -> Form
subTree cone s i = 
  case s of
    n::xs -> List.range 0 (n - 1)
     |> List.map (branchFork n (cone*opt.coneDamping) xs) 
     |> group
    _ -> fruit (opt.fruitHueScale*(toFloat i) + opt.fruitHueOffs)


ptree: List Int -> (Int, Int) -> Element         
ptree factors (w, h) = 
    collage w h [subTree opt.branchCone factors 0
                 |> moveY (toFloat (-h//2))
                 |> scale ((toFloat (min h w))/(toFloat opt.refH))]

    
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
        |> Text.height (toFloat opt.fontHeight)
        |> centered

           
primeView: Model -> Html Msg
primeView {num, paused, windowWidth, windowHeight} =
    let factors = primes num in
    div []
    [flow down
      [caption num factors |> width windowWidth,
       layers [
        ptree factors (windowWidth, windowHeight - opt.captionHeight),
        (if paused then "[PAUSED] " else "")
             ++ "SPACE - toggle pause, ARROWS - prev/next"
          |> Text.fromString
          |> Text.color Color.grey
          |> centered
          |> width windowWidth
       ]] |> toHtml]




