{-# LANGUAGE ScopedTypeVariables #-}
module Game where

-- import qualified

import ECS.Base
import qualified Utils
import qualified ECS.Manager
import qualified TileMap
import qualified ECS.SpriteComponent as Sprite
import qualified ECS.KeyboardController
import qualified EventHandler
import qualified PhysicsEngine
import qualified Constants
import qualified TextureManager
import qualified Init


import Apecs
import Control.Monad (unless)
import qualified Data.HashMap.Strict as HM
import qualified SDL
import Linear
import Data.Foldable
import Foreign.C.Types (CInt, CFloat)
import Data.Text (Text)


init :: Text -> V2 CInt -> V2 CInt -> Bool -> System' ()
init title pos size fullscr = do
    SDL.initializeAll
    Utils.consoleLog "Subsystems Initialized!"

    window <- SDL.createWindow title $
      SDL.defaultWindow { SDL.windowPosition    = SDL.Absolute $ SDL.P pos
                        , SDL.windowInitialSize = size
                        , SDL.windowMode        = if fullscr 
                                                    then SDL.Fullscreen 
                                                    else SDL.Windowed
                        }
    global $= Window window
    Utils.consoleLog "Window created!"
    
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    global $= Renderer renderer
    Utils.consoleLog "Renderer created!"

    global $= (Running True, Time 0)
    set global =<< TileMap.initMap

    TextureManager.loadTexture "assets/DEFAULT.png"
    Init.initPlayer


clean :: System' ()
clean = do
  cmap $ \(Active _) -> Active False
  ECS.Manager.refresh

  (  Window window
   , Renderer renderer
   , Textures texMap
   ) <- get global
  traverse_ SDL.destroyTexture $ HM.elems texMap

  SDL.destroyWindow window
  SDL.destroyRenderer renderer
  SDL.quit
  Utils.consoleLog "Game Cleaned!"


-- GAME LOOP FUNCTIONS --


handleEvents :: System' ()
handleEvents = do
  payload <- map SDL.eventPayload <$> SDL.pollEvents 
  mapM_ EventHandler.handleEvent payload
  ECS.KeyboardController.keyboardHandle


update :: System' ()
update = do
  Time t <- get global
  t' <- Utils.toSeconds <$> SDL.ticks
  let dT = t' - t
  global $= (Time t', DT dT)
  PhysicsEngine.update
  ECS.Manager.update
  ECS.KeyboardController.updatePrevControls


draw :: System' ()
draw = do
  Renderer renderer <- get global
  SDL.clear renderer
  TileMap.drawMap
  ECS.Manager.draw
  
  SDL.rendererDrawColor renderer SDL.$= V4 255 255 255 255
  SDL.present renderer
