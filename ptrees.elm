module Ptree where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Signal exposing (..)
import List exposing (..)

goldenPhi: Float
goldenPhi = 137.5/180.0*pi

opt = { trunkClr = hsl 0.3 0.5 0.4, 
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

caption: Int -> List Int -> Element
caption n factors = 
    show (toString n ++  
         (if length factors == 2 
          then " - PRIME!" 
          else " = " ++ toString (drop 1 factors)))

primeView: Int -> Element
primeView n = 
    let factors = primes n in           
    flow down [caption n factors, ptree factors]

main: Signal Element
main = 
    primeView 
    << (\n -> (n//opt.animDelay)%(opt.maxNum)) 
    << round 
    << inSeconds 
    <~ every second

fruitGrad: Float -> Gradient
fruitGrad h = 
    radial (0, 0) 3 
           (4, -4) 16
           [(0, hsl h 0.5 0.8), 
            (1, hsl h 0.5 0.5)]

primeFactors: Int -> Int -> List Int
primeFactors n s = 
  if | s*s > n -> [n]
     | n%s == 0 -> s::(primeFactors (n//s) s)
     | s == 2 -> primeFactors n (s + 1)
     | otherwise -> primeFactors n (s + 2)

primes: Int -> List Int
primes n = 1::(primeFactors n 2)

