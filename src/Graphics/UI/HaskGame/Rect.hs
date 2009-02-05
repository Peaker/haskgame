module Graphics.UI.HaskGame.Rect
    (Rect(Rect)
    ,rectToVectors
    ,vectorsToRect
    ,makeRect
    ,vectorsToPVectors, pVectorsToVectors, rectPVectors
    ,inRect
    ,rectPos, rectSize
    ,rectX, rectY, rectW, rectH
    ,makePosRect
    ,posInRect
    ,unionRects
    ,intersectRects)
where

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL(Rect(Rect))
import Graphics.UI.HaskGame.Vector2(Vector2(..), vector2first, vector2second)
import Control.Arrow(first, second)
import Control.Applicative(liftA2)

type Endo a = a -> a
type Two a = (a, a)

rectToVectors :: Rect -> Two (Vector2 Int)
rectToVectors (SDL.Rect x y w h) = (Vector2 x y, Vector2 w h)

vectorsToRect :: Two (Vector2 Int) -> Rect
vectorsToRect (Vector2 x y, Vector2 w h) = SDL.Rect x y w h

makeRect :: Vector2 Int -> Vector2 Int -> Rect
makeRect = curry vectorsToRect

-- Rect to Positional vectors
vectorsToPVectors, pVectorsToVectors :: Endo (Two (Vector2 Int))
vectorsToPVectors (p, s) = (p, p+s)
pVectorsToVectors (p1, p2) = (p1, p2-p1)

rectPVectors :: Rect -> Two (Vector2 Int)
rectPVectors = vectorsToPVectors . rectToVectors

inRect :: Endo (Two (Vector2 Int)) -> Endo Rect
inRect f = vectorsToRect . f . rectToVectors

rectPos, rectSize :: Endo (Vector2 Int) -> Endo Rect
rectPos = inRect . first
rectSize = inRect . second

rectX, rectY, rectW, rectH :: Endo Int -> Endo Rect
rectX = rectPos . vector2first
rectY = rectPos . vector2second
rectW = rectSize . vector2first
rectH = rectSize . vector2second

makePosRect :: Vector2 Int -> Rect
makePosRect (Vector2 x y) = SDL.Rect x y 0 0

posInRect :: Vector2 Int -> Rect -> Bool
posInRect pos rect =
    let (rpos, rsize) = rectToVectors rect
    in pos-rpos < rsize

unionRects :: Rect -> Rect -> Rect
unionRects r1 r2 =
    let (tl1, br1) = rectPVectors r1
        (tl2, br2) = rectPVectors r2
    in vectorsToRect . pVectorsToVectors $
           ((liftA2 min tl1 tl2),
            (liftA2 max br1 br2))

intersectRects :: Rect -> Rect -> Rect
intersectRects r1 r2 =
    let (Vector2 left1 top1,
         Vector2 right1 bottom1) = rectPVectors r1
        (Vector2 left2 top2,
         Vector2 right2 bottom2) = rectPVectors r2
        left = max left1 left2
        top = max top1 top2
        right = min right1 right2
        bottom = min bottom1 bottom2
        width = right - left
        height = bottom - top
    in Rect left top width height
