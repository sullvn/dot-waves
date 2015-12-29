module Main where

import Prelude

import Math (sin)

import Data.Maybe (Maybe(..))
import Data.Array ((..))
import Data.Foldable (fold)
import Data.Int (toNumber)

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
import Graphics.Drawing.Color (rgb)
import Graphics.Drawing (
  render, translate,
  Drawing(), Shape(),
  circle, fillColor, filled
)

canvasConfig :: { boundaries :: Rectangle }
canvasConfig = {
  boundaries: {
    x: 0.0, y: 0.0,
    w: 800.0, h: 500.0
  }
}

dotConfig = {
  separation: spacing + maxSize,
  horizontal: 20,
  vertical: 15,

  maxSize,
  minSize: 5.0,
  color: rgb 200.0 220.0 210.0
}
  where
    spacing = 5.0
    maxSize = 20.0

graphicsOrigin = {
  x: (canvasConfig.boundaries.w - ((toNumber dotConfig.horizontal) * dotConfig.separation)) / 2.0,
  y: (canvasConfig.boundaries.h - ((toNumber dotConfig.vertical) * dotConfig.separation)) / 2.0
}

renderDot :: Number -> Number -> Number -> Drawing
renderDot x y r = 
  filled (fillColor dotConfig.color) dot
  where
    dot :: Shape
    dot = circle x y r

renderDots :: Int -> Int -> Number -> Number -> Drawing
renderDots xn yn spacing diameter = fold dots
  where 
    places :: Array { x :: Int, y :: Int }
    places = (\x y -> { x, y }) <$> 0 .. (xn - 1) <*> 0 .. (yn - 1)

    coord { x, y } = { x: x' * spacing, y: y' * spacing }
      where x' = toNumber x
            y' = toNumber y

    coords :: Array { x :: Number, y :: Number }
    coords = coord <$> places

    dot { x, y } = renderDot x y (diameter / 2.0)
    dots = dot <$> coords

pulse :: forall eff. Number -> Number -> Eff (dom :: DOM, timer :: Timer | eff) (Signal Number)
pulse min max = do
  nowMs <- animationFrame
  return $ nowMs ~> amp
    where secs ms = ms / 1000.0
          unitAmp x = ((sin x) + 1.0) / 2.0
          amp ms = (unitAmp $ secs ms) * (max - min) + min


rendering :: forall eff. Context2D -> Number -> Eff (canvas :: Canvas | eff) Unit
rendering ctx n = do
  let dotGraphics = renderDots dotConfig.horizontal dotConfig.vertical dotConfig.separation n
      graphics = translate graphicsOrigin.x graphicsOrigin.y dotGraphics

  clearRect ctx canvasConfig.boundaries
  render ctx graphics 

main :: forall eff. Eff ( timer :: Timer, dom :: DOM, canvas :: Canvas | eff) Unit
main = do
  Just canvas <- getCanvasElementById "dot-waves"
  ctx <- getContext2D canvas
  p <- pulse dotConfig.minSize dotConfig.maxSize
  runSignal $ p ~> (rendering ctx)
