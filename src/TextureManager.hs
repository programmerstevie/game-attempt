module TextureManager where

-- import qualified

import ECS.Base

import Foreign.C.Types (CInt)
import Apecs
import qualified SDL
import qualified SDL.Image as IMG
import Control.Monad.IO.Class


loadTexture :: FilePath -> System' SDL.Texture
loadTexture path = do
  Renderer renderer <- get global
  tempSurface <- IMG.load path
  tex <- SDL.createTextureFromSurface renderer tempSurface
  SDL.freeSurface tempSurface
  pure tex


draw :: SDL.Texture 
     -> Maybe (SDL.Rectangle CInt) 
     -> Maybe (SDL.Rectangle CInt) 
     -> System' ()
draw tex src dest = do
  Renderer renderer <- get global
  SDL.copy renderer tex src dest
