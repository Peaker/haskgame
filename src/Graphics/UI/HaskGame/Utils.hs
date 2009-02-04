{-# OPTIONS -Wall -O2 #-}

module Graphics.UI.HaskGame.Utils
    (Size, bracket__,ioBoolToError)
where

import qualified IO
import Graphics.UI.HaskGame.Vector2(Vector2)

type Size = Vector2 Int

bracket__ :: IO () -> IO () -> IO () -> IO ()
bracket__ pre post code = IO.bracket_ pre (const post) code

ioBoolToError :: String -> IO Bool -> IO ()
ioBoolToError errStr act = do
  isSuccess <- act
  if isSuccess
    then return ()
    else error errStr
-- Commented out for 6.8's lack of the new Exception
--       throwIO . userError $ errStr
