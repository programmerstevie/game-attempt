{-# LANGUAGE ScopedTypeVariables #-}
module TileMap where

import ECS.Base
import qualified TextureManager
import qualified Constants as Cons
import qualified Utils
import qualified Camera
import qualified Renderer

import Apecs
import Linear
import qualified Data.Array as Array
import qualified Data.HashMap.Strict as HM
import Foreign.C.Types (CInt)
import Control.Monad



lvl1 :: MapTiles
lvl1 = Array.listArray (0, V2 19 30) (
     [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
  ++ [2, 2, 2, 2, 3, 3, 3, 1, 1, 1, 1, 1, 3, 3, 3, 1, 1, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2]
  ++ [2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2]
  ++ [2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 2]
  ++ [2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2]
  ++ [2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
  )


initMap :: System' TMap
initMap = do
  TextureManager.loadTextures 
    [ "assets/dirt.png"
    , "assets/grass.png"
    , "assets/oneWay.png"
    , "assets/Background2048x1536.png"
    ]
  return $ TMap{
    map_M      = lvl1
  , srcRect_M  = Just $ Utils.makeRect 0 (V2 32 32)
  , destRect_M = Just $ Utils.makeRect 0 (V2 32 32)
  , playerStartPos_M = V2 0 7
  , exitPos_M = 0
  , ents_M = []
  }

loadMap :: MapTiles -> System' ()
loadMap arr = do
  tileMap :: TMap <- get global
  global $= tileMap{ map_M = arr }
  
drawMap :: System' ()
drawMap = do
  Textures texMap <- get global
  let defaultTex = texMap HM.! "assets/DEFAULT.png"
      bkgr =
        HM.lookupDefault defaultTex "assets/Background2048x1536.png" texMap
  Renderer.setCamViewPort
  TextureManager.draw bkgr Nothing Nothing Cons.noFlip
  TMap{ destRect_M = destR
      , map_M      = mapTiles
      } <- get global
  let (_, V2 mapH mapW) = Array.bounds mapTiles
  forM_ (Array.assocs mapTiles) $ \(coords, tile) -> do
    destR' <- Utils.setRectPos destR <$> Camera.mapToSdlCoords mapTiles coords
    let
      path = case tile of
                1 -> "assets/grass.png"
                2 -> "assets/dirt.png"
                3 -> "assets/oneWay.png"
                _ -> ""
    when (tile > 0) $
      TextureManager.draw 
        (HM.lookupDefault defaultTex path texMap) Nothing destR' Cons.noFlip
