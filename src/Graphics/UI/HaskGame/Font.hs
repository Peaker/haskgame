{-# OPTIONS -Wall -O2 #-}

module Graphics.UI.HaskGame.Font
    (Font
    ,renderText,textSize
    ,defaultFont,withInit)
where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import qualified Graphics.UI.HaskGame.Utils as Utils
import Graphics.UI.HaskGame.Color(Color(..))
import Graphics.UI.HaskGame.Vector2(Vector2(..))
import System.IO.Unsafe(unsafePerformIO)

type Font = TTF.Font

renderText :: Font -> String -> Color -> IO SDL.Surface
renderText font text (Color r g b) = if null text
                             then
                                 SDL.createRGBSurface [] 0 0 0 0 0 0 0
                             else
                                 TTF.renderTextBlended font text (SDL.Color r g b)

textSize :: Font -> String -> Vector2 Int
textSize font text =
    let (w, h) = unsafePerformIO $ TTF.textSize font text
    in Vector2 w h

defaultFont :: Int -> IO Font
defaultFont = TTF.openFont "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

withInit :: IO () -> IO ()
withInit = Utils.bracket__ (Utils.ioBoolToError "TTF init failure" TTF.init) TTF.quit
