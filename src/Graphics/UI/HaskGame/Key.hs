{-# OPTIONS_GHC -Wall -O2 #-}

module Graphics.UI.HaskGame.Key
    (keyOfEvent
    ,ModKey(..),KeyGroup(..)
    ,singletonKeyGroup,asKeyGroup,keyName
    ,Mods(..),noMods,shift,ctrl,alt
    ,Keysym)
where

import qualified Graphics.UI.SDL as SDL
import qualified Data.Set as Set

type Keysym = SDL.Keysym
data Mods = MkMods { isShift, isCtrl, isAlt :: Bool }
  deriving (Eq, Ord, Show, Read)
data ModKey = ModKey Mods SDL.SDLKey
  deriving (Eq, Ord, Show)
data KeyGroup = KeyGroup {
      keyGroupName :: String
    , keyGroupKeys :: Set.Set ModKey
}
  deriving (Eq, Ord, Show)

singletonKeyGroup :: ModKey -> KeyGroup
singletonKeyGroup key = KeyGroup (keyName key) (Set.singleton key)

asKeyGroup :: Mods -> SDL.SDLKey -> KeyGroup
asKeyGroup = (fmap . fmap) singletonKeyGroup ModKey

modsName :: Mods -> String
modsName mods =
    let shiftStr = if isShift mods then "Shift+" else ""
        ctrlStr  = if isCtrl mods then "Ctrl+" else ""
        altStr   = if isAlt mods then "Alt+" else ""
    in concat [shiftStr, ctrlStr, altStr]

keyName :: ModKey -> String
keyName (ModKey mods sdlkey) = modsName mods ++ SDL.getKeyName sdlkey

noMods :: Mods
noMods = MkMods False False False

shift :: Mods
shift = noMods{isShift=True}

ctrl :: Mods
ctrl = noMods{isCtrl=True}

alt :: Mods
alt = noMods{isAlt=True}

modsOf :: [SDL.Modifier] -> Mods
modsOf mods =
    MkMods (any (`elem` mods)
            [SDL.KeyModLeftShift,
             SDL.KeyModRightShift,
             SDL.KeyModShift])
           (any (`elem` mods)
            [SDL.KeyModLeftCtrl,
             SDL.KeyModRightCtrl,
             SDL.KeyModCtrl])
           (any (`elem` mods)
            [SDL.KeyModLeftAlt,
             SDL.KeyModRightAlt,
             SDL.KeyModAlt])

keyOfEvent :: Keysym -> ModKey
keyOfEvent keySym = ModKey (modsOf $ SDL.symModifiers keySym)
                           (SDL.symKey keySym)
