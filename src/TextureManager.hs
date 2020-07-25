module TextureManager where

-- import qualified

import ECS.Base
import qualified Camera

import Foreign.C.Types (CInt)
import Apecs
import Data.Foldable
import Linear
import qualified Data.HashMap.Strict as HM
import qualified SDL
import qualified SDL.Image as IMG


loadTexture :: FilePath -> System' SDL.Texture
loadTexture path = do
  CRenderer renderer <- get global
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
     -> V2 Bool
     -> System' ()
draw tex src dest flp = do
  renderer <- unRenderer <$> get global
  dest' <- Camera.scaleRecToCamera dest
  SDL.copyEx renderer tex src dest' 0 Nothing flp

