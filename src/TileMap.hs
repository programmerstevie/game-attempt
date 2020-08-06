{-# LANGUAGE ScopedTypeVariables #-}
module TileMap where

import ECS.Base
import qualified TextureManager
import qualified Constants as Cons
import qualified Data.HashMap.Strict as HM
import qualified Utils
import qualified Camera
import qualified Renderer

import Apecs
import Data.Aeson
import Data.Maybe (fromMaybe)
import qualified Data.Array as Array
import Control.Monad


loadMap :: FilePath -> System' ()
loadMap fp = do
  tm :: Maybe TMap <- liftIO $ decodeFileStrict' fp
  let tileMap = fromMaybe (error "tileMap could not be loaded.") tm
  TextureManager.loadTextures [ backgroundPath_M tileMap 
                              , tileTexturePath_M tileMap 
                              ]
  global $= tileMap


drawMap :: System' ()
drawMap = do
  Renderer.setCamViewPort
  TMap{ backgroundPath_M  = bkgrPath
      , tileTexturePath_M = texPath
      , tileTextureMap_M  = texMap
      , destRect_M        = destRectRaw
      , map_M             = mapTiles
      } <- get global
  bkgr <- TextureManager.getTexture bkgrPath
  tileTex <- TextureManager.getTexture texPath

  TextureManager.draw bkgr Nothing Nothing Cons.noFlip
  forM_ (Array.assocs mapTiles) $ \(coords, tile) -> do
    let srcRect = texMap HM.! tile
    destRect <- Camera.scaleRecToCamera =<< 
      Utils.setRectPos destRectRaw <$> Camera.mapToSdlCoords mapTiles coords
    TextureManager.draw tileTex srcRect destRect Cons.noFlip
