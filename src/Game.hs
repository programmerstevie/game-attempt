{-# LANGUAGE ScopedTypeVariables #-}
module Game where

-- import qualified

import ECS.Base
import qualified Utils
import qualified ECS.Manager
import qualified TileMap
import qualified ECS.KeyboardController
import qualified EventHandler
import qualified PhysicsEngine
import qualified TextureManager
import qualified Init
import qualified Renderer
import qualified Camera


import Apecs
import qualified Data.HashMap.Strict as HM
import qualified SDL
import Linear
import Data.Foldable
import Foreign.C.Types (CInt)
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
  global $= CWindow window
  Utils.consoleLog "Window created!"
  
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  global $= CRenderer renderer
  Utils.consoleLog "Renderer created!"

  global $= Camera 0 (V2 31 20)

  global $= (Running True, Time 0)
  set global =<< TileMap.initMap

  TextureManager.loadTextures [ "assets/DEFAULT.png"
                              , "assets/Background2048x1536.png"
                              ]
  Init.initPlayer


clean :: System' ()
clean = do
  cmap $ \(Active _) -> Active False
  ECS.Manager.refresh

  (  CWindow window
   , CRenderer renderer
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
  ECS.KeyboardController.updatePrevControls
  payload <- map SDL.eventPayload <$> SDL.pollEvents 
  mapM_ EventHandler.handleEvent payload
  ECS.KeyboardController.keyboardHandle


update :: System' ()
update = do
  Time t <- get global
  t' <- Utils.toSeconds <$> SDL.ticks
  let dT = t' - t
  global $= (Time t', DT dT)
  ECS.Manager.refresh
  ECS.Manager.actionUpdate
  PhysicsEngine.update
  Camera.updateCamera
  ECS.Manager.updateSprites


draw :: System' ()
draw = do
  CRenderer renderer <- get global
  SDL.clear renderer
  TileMap.drawMap
  ECS.Manager.draw
  
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255
  Renderer.setFullViewPort
  SDL.present renderer