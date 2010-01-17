{-# OPTIONS -Wall -O2 #-}

module Graphics.UI.HaskGame
    (Surface,Event
    ,createRGBSurface,blit,blitPart,fillRect,fillSurface
    ,withInit,getEvents,surfaceSize,setVideoMode
    )
where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.HaskGame.Rect as Rect
import qualified Graphics.UI.HaskGame.Font as Font
import qualified Graphics.UI.HaskGame.Utils as Utils
import Graphics.UI.HaskGame.Rect(Rect)
import Graphics.UI.HaskGame.Color(Color(..))
import Graphics.UI.HaskGame.Vector2(Vector2(..))
import Control.Monad(liftM)

-- Commented out for 6.8's lack of the new Exception
-- import Control.Exception(throwIO)

type Surface = SDL.Surface
type Event = SDL.Event

whileM :: Monad m => (a -> Bool) -> m a -> m [a]
whileM cond element = do
  value <- element
  if cond value
    then liftM (value:) (whileM cond element)
    else return []

createRGBSurface :: Vector2 Int -> IO Surface
createRGBSurface (Vector2 w h) = do
  surface <- SDL.createRGBSurface [SDL.SWSurface]
     w h 32 0xFF 0xFF00 0xFF0000 0x00000000
  SDL.displayFormatAlpha surface

sdlCall :: String -> IO Bool -> IO ()
sdlCall name act = do
  res <- act
  if not res
    then
      fail ("SDL " ++ name ++ " failed")
    else
      return ()

sdlBlitSurface :: SDL.Surface
               -> Maybe Rect
               -> SDL.Surface
               -> Maybe Rect
               -> IO ()
sdlBlitSurface src srcRect dest destRect =
    sdlCall "blitSurface" $ SDL.blitSurface src srcRect dest destRect

blit :: Surface -> Vector2 Int -> Surface -> IO ()
blit dest pos src = sdlBlitSurface src Nothing dest (Just . Rect.makeFromPos $ pos)

blitPart :: Surface -> Vector2 Int -> Surface -> Rect -> IO ()
blitPart dest destPos src srcRect =
    sdlBlitSurface
        src  (Just . Rect.trunc $ srcRect)
        dest (Just . Rect.makeFromPos $ destPos)

sdlFillRect :: Surface -> Maybe Rect -> Color -> IO ()
sdlFillRect surface mRect color = do
  fillerPixel <- pixel surface color
  sdlCall "fillRect" $ SDL.fillRect surface mRect fillerPixel

fillSurface :: Surface -> Color -> IO ()
fillSurface surface color = sdlFillRect surface Nothing color

fillRect :: Surface -> Rect -> Color -> IO ()
fillRect surface rect color = sdlFillRect surface (Just rect) color

initKeyRepeat :: IO ()
initKeyRepeat = Utils.ioBoolToError "enableKeyRepeat failed" $ SDL.enableKeyRepeat 150 10

withInit :: IO () -> IO ()
withInit = SDL.withInit [SDL.InitEverything] .
           Utils.bracket__ initKeyRepeat (return ()) .
           Font.withInit

pixel :: Surface -> Color -> IO SDL.Pixel
pixel surface (Color r g b) = SDL.mapRGB (SDL.surfaceGetPixelFormat surface) r g b

getEvents :: IO [Event]
getEvents = whileM (/=SDL.NoEvent) SDL.pollEvent

surfaceSize :: Surface -> Vector2 Int
surfaceSize surface = Vector2 (SDL.surfaceGetWidth surface)
                              (SDL.surfaceGetHeight surface)

setVideoMode :: Vector2 Int -> Int -> IO Surface
setVideoMode (Vector2 xres yres) colorDepth = SDL.setVideoMode xres yres colorDepth [SDL.DoubleBuf]
