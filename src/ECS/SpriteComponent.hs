module ECS.SpriteComponent where

-- import qualified as Sprite

import qualified TextureManager
import qualified Utils
import ECS.Base


import qualified SDL
import Linear
import Foreign.C.Types (CInt)


init :: FilePath -> CInt -> CInt -> System' Sprite
init path width height = do
  tex <- TextureManager.loadTexture path

  SDL.TextureInfo{ SDL.textureWidth  = w
                 , SDL.textureHeight = h } <- SDL.queryTexture tex
  let srcRect  = Utils.makeRect 0 (V2 w h)
      destRect = Utils.makeRect 0 (V2 width height)
  pure Sprite{ filePath_S = path
             , srcRect_S  = Just srcRect
             , destRect_S = Just destRect
             , flip_S     = V2 False False
             }


setTex :: Sprite -> FilePath -> System' Sprite
setTex spr path = do
  tex <- TextureManager.loadTexture path
  SDL.TextureInfo{ SDL.textureWidth  = w
                 , SDL.textureHeight = h } <- SDL.queryTexture tex
  let srcRect  = Utils.makeRect 0 (V2 w h)
  pure spr{ filePath_S = path
          , srcRect_S  = Just srcRect
          }
