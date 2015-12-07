module Main where

import Prelude

import Math (sin)
import Data.Maybe (Maybe(..))

import Control.Monad.Eff (Eff())
import Control.Timer (Timer())

import Signal ((~>), Signal(), runSignal)
import Signal.DOM (animationFrame)

import DOM (DOM())

import Graphics.Canvas (
  Canvas(), Context2D(),
  getCanvasElementById, getContext2D, clearRect,
  Rectangle()
)
import Graphics.Drawing.Color (Color(), rgb)
import Graphics.Drawing (
  render,
  Drawing(), Shape(),
  circle, fillColor, filled
)

boundaries :: Rectangle
boundaries = {
  x: 0.0, y: 0.0,
  w: 800.0, h: 500.0
}

renderDot :: Number -> Number -> Number -> Drawing
renderDot x y r = 
  filled (fillColor color) dot
  where
    dot :: Shape
    dot = circle x y r

    color :: Color
    color = rgb 200.0 220.0 210.0

pulse :: forall eff. Number -> Number -> Eff (dom :: DOM, timer :: Timer | eff) (Signal Number)
pulse min max = do
  nowMs <- animationFrame
  return $ nowMs ~> amp
    where secs ms = ms / 1000.0
          unitAmp x = ((sin x) + 1.0) / 2.0
          amp ms = (unitAmp $ secs ms) * (max - min) + min


rendering :: forall eff. Context2D -> Number -> Eff (canvas :: Canvas | eff) Unit
rendering ctx n = do
  clearRect ctx boundaries
  render ctx $ renderDot 400.0 250.0 n

main :: forall eff. Eff ( timer :: Timer, dom :: DOM, canvas :: Canvas | eff) Unit
main = do
  Just canvas <- getCanvasElementById "dot-waves"
  ctx <- getContext2D canvas
  p <- pulse 30.0 100.0
  runSignal $ p ~> (rendering ctx)
