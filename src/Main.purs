module Main where

import Prelude

import Math (sin, pow)

import Data.Maybe (fromJust)
import Data.Array ((..))
import Data.Foldable (fold)
import Data.Int (toNumber, floor)

import Control.Monad.Eff (Eff())
import Control.Timer (TIMER())

import Signal ((~>), Signal(), runSignal)
import Signal.DOM (animationFrame)

import DOM (DOM())

import Graphics.Canvas (
  CANVAS(), Context2D(),
  getCanvasElementById, getContext2D, clearRect,
  Rectangle()
)
import Color (rgb)
import Graphics.Drawing (
  render, translate,
  Drawing(), Shape(),
  circle, fillColor, filled
)

canvasConfig :: { boundaries :: Rectangle }
canvasConfig = {
  boundaries: {
    x: 0.0, y: 0.0,
    w: 1400.0, h: 800.0
  }
}

dotConfig = {
  separation: spacing + maxSize,
  horizontal: 50,
  vertical: 30,

  maxSize,
  minSize: 5.0,
  color: rgb 200 220 210
}
  where
    spacing = 5.0
    maxSize = 20.0

graphicsOrigin :: { x :: Number, y :: Number }
graphicsOrigin = {
  x: (canvasConfig.boundaries.w - ((toNumber dotConfig.horizontal) * dotConfig.separation)) / 2.0,
  y: (canvasConfig.boundaries.h - ((toNumber dotConfig.vertical) * dotConfig.separation)) / 2.0
}

renderDot :: Int -> Int -> Number -> Drawing
renderDot x y t = 
  filled (fillColor color) dot
  where
    xp = (toNumber x) * dotConfig.separation
    yp = (toNumber y) * dotConfig.separation

    xf = (toNumber x) / (toNumber dotConfig.horizontal)
    yf = (toNumber y) / (toNumber dotConfig.vertical)

    t' = t + (xf * yf * 8.0)
    unitAmp n = ((sin n) + 1.0) / 2.0
    diameter = (pow (unitAmp t') 3.0) * (dotConfig.maxSize - dotConfig.minSize) + dotConfig.minSize

    clrCmp n off = floor $ off + ((unitAmp (t' + n)) * (200.0 - off))
    color = rgb (clrCmp 0.0 40.0) (clrCmp 0.66 25.0) (clrCmp 0.33 25.0)

    dot :: Shape
    dot = circle xp yp (diameter / 2.0)

renderDots :: Int -> Int -> Number -> Drawing
renderDots xn yn t = fold dots
  where 
    places :: Array { x :: Int, y :: Int }
    places = (\x y -> { x, y }) <$> 0 .. (xn - 1) <*> 0 .. (yn - 1)

    dot { x, y } = renderDot x y t
    dots = dot <$> places

seconds :: forall eff. Eff (dom :: DOM, timer :: TIMER | eff) (Signal Number)
seconds = do
  nowMs <- animationFrame
  pure $ nowMs ~> secs
    where secs ms = ms / 1000.0


rendering :: forall eff. Context2D -> Number -> Eff (canvas :: CANVAS | eff) Unit
rendering ctx n = do
  let dotGraphics = renderDots dotConfig.horizontal dotConfig.vertical n
      graphics = translate graphicsOrigin.x graphicsOrigin.y dotGraphics

  clearRect ctx canvasConfig.boundaries
  render ctx graphics 

main :: forall eff. Partial => Eff ( timer :: TIMER, dom :: DOM, canvas :: CANVAS | eff) Unit
main = do
  maybeCanvas <- getCanvasElementById "dot-waves"
  let canvas = fromJust maybeCanvas
  ctx <- getContext2D canvas
  s <- seconds
  runSignal $ s ~> (rendering ctx)
