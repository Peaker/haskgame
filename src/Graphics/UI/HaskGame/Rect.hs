module Graphics.UI.HaskGame.Rect
    (Rect(Rect)
    ,toVectors
    ,fromVectors
    ,make
    ,getTopLeft
    ,getSize
    ,getBottomRight
    ,vectorsToPVectors, pVectorsToVectors, toPVectors
    ,inRect
    ,pos, size
    ,x, y, w, h
    ,makeFromPos,makeFromSize
    ,posInside
    ,trunc
    ,union
    ,intersect)
where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.HaskGame.Vector2 as Vector2
import Graphics.UI.SDL(Rect(Rect))
import Graphics.UI.HaskGame.Vector2(Vector2(..))
import Control.Arrow(first, second)
import Control.Applicative(liftA2)

type Endo a = a -> a
type Two a = (a, a)

toVectors :: Rect -> Two (Vector2 Int)
toVectors (SDL.Rect rx ry rw rh) = (Vector2 rx ry, Vector2 rw rh)

getTopLeft :: Rect -> Vector2 Int
getTopLeft = fst . toVectors

getSize :: Rect -> Vector2 Int
getSize = snd . toVectors

getBottomRight :: Rect -> Vector2 Int
getBottomRight = snd . vectorsToPVectors . toVectors

fromVectors :: Two (Vector2 Int) -> Rect
fromVectors (Vector2 rx ry, Vector2 rw rh) = SDL.Rect rx ry rw rh

make :: Vector2 Int -> Vector2 Int -> Rect
make = curry fromVectors

-- To Positional vectors
vectorsToPVectors :: Endo (Two (Vector2 Int))
vectorsToPVectors (p, s) = (p, p+s)

pVectorsToVectors :: Endo (Two (Vector2 Int))
pVectorsToVectors (p1, p2) = (p1, p2-p1)

toPVectors :: Rect -> Two (Vector2 Int)
toPVectors = vectorsToPVectors . toVectors

inRect :: Endo (Two (Vector2 Int)) -> Endo Rect
inRect f = fromVectors . f . toVectors

pos :: Endo (Vector2 Int) -> Endo Rect
pos = inRect . first

size :: Endo (Vector2 Int) -> Endo Rect
size = inRect . second

x :: Endo Int -> Endo Rect
x = pos . Vector2.first

y :: Endo Int -> Endo Rect
y = pos . Vector2.second

w :: Endo Int -> Endo Rect
w = size . Vector2.first

h :: Endo Int -> Endo Rect
h = size . Vector2.second

makeFromPos :: Vector2 Int -> Rect
makeFromPos (Vector2 rx ry) = SDL.Rect rx ry 0 0

makeFromSize :: Vector2 Int -> Rect
makeFromSize (Vector2 rw rh) = SDL.Rect 0 0 rw rh

posInside :: Vector2 Int -> Rect -> Bool
posInside point rect =
    let (rpoint, rsize) = toVectors rect
    in point-rpoint < rsize

-- Truncate negative sizes to 0
trunc :: Rect -> Rect
trunc = w (max 0) . h (max 0)

union :: Rect -> Rect -> Rect
union r1 r2 =
    let (tl1, br1) = toPVectors r1
        (tl2, br2) = toPVectors r2
    in fromVectors . pVectorsToVectors $
           ((liftA2 min tl1 tl2),
            (liftA2 max br1 br2))

intersect :: Rect -> Rect -> Rect
intersect r1 r2 =
    let (Vector2 left1 top1,
         Vector2 right1 bottom1) = toPVectors r1
        (Vector2 left2 top2,
         Vector2 right2 bottom2) = toPVectors r2
        left = max left1 left2
        top = max top1 top2
        right = min right1 right2
        bottom = min bottom1 bottom2
        width = right - left
        height = bottom - top
    in Rect left top width height
