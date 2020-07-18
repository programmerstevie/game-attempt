module TextureManager where

-- import qualified

import ECS.Base

import Foreign.C.Types (CInt)
import Apecs
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified SDL
import qualified SDL.Image as IMG


loadTexture :: FilePath -> System' SDL.Texture
loadTexture path = do
  Renderer renderer <- get global
  tempSurface <- IMG.load path
  tex <- SDL.createTextureFromSurface renderer tempSurface
  SDL.freeSurface tempSurface
  global $~ \(Textures texMap) -> Textures $ HM.insert path tex texMap
  pure tex

loadTextures :: [FilePath] -> System' ()
loadTextures = traverse_ loadTexture


draw :: SDL.Texture 
     -> Maybe (SDL.Rectangle CInt) 
     -> Maybe (SDL.Rectangle CInt) 
     -> System' ()
draw tex src dest = do
  Renderer renderer <- get global
  SDL.copy renderer tex src dest
