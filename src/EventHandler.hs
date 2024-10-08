module EventHandler where


import ECS.Base
import qualified KeyboardController


import Apecs
import qualified SDL


handleEvent :: SDL.EventPayload -> System' ()
handleEvent (SDL.KeyboardEvent ev) = handleKeyEvent ev
handleEvent SDL.QuitEvent = global $= Running False
handleEvent _ = pure ()


handleKeyEvent :: SDL.KeyboardEventData -> System' ()
handleKeyEvent ev = do
  let keysym    = SDL.keyboardEventKeysym ev
      motion    = SDL.keyboardEventKeyMotion ev
      --keycode   = SDL.keysymKeycode  keysym
      scancode  = SDL.keysymScancode keysym
  KeyboardController.setControls scancode motion
