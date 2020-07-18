{-# LANGUAGE LambdaCase #-}
module TileMap where

import ECS.Base
import qualified TextureManager
import qualified Utils

import Apecs
import Linear
import qualified Data.Array as Array
import qualified Data.HashMap.Strict as HM
import Foreign.C.Types (CInt)
import Control.Monad


lvl1 :: MapTiles
lvl1 = Array.listArray (0, V2 19 24) (
     [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 2, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 2, 2, 2, 0, 0, 0, 0, 0]
  ++ [0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 0, 0, 0, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0]
  ++ [1, 1, 1, 1, 0, 0, 0, 2, 2, 2, 2, 2, 0, 0, 0, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0]
  ++ [2, 2, 2, 2, 0, 0, 0, 2, 2, 2, 2, 2, 0, 0, 0, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0]
  ++ [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
  )


initMap :: System' TMap
initMap = do
  TextureManager.loadTextures ["assets/dirt.png", "assets/grass.png"]
  return $ TMap{
    srcRect_M  = Just $ Utils.makeRect 0 (V2 32 32)
  , destRect_M = Just $ Utils.makeRect 0 (V2 32 32)
  , map_M      = Just lvl1
  }

loadMap :: MapTiles -> System' ()
loadMap arr = get global >>= \case
  TMapNULL -> pure ()
  tileMap  -> global $= tileMap{ map_M = Just arr }
  
drawMap :: System' ()
drawMap = get global >>= \case
  TMapNULL  -> pure ()
  TMap{ destRect_M = destR
      , map_M      = maybe_arr
      } -> case maybe_arr of
        Nothing  -> pure ()
        Just arr ->
          forM_ (Array.assocs arr) $ \(V2 row column, tile) -> do
            let destR' = Utils.setRectPos destR $ 
                          fromIntegral <$> V2 (column * 32) (row * 32)
            Textures texMap <- get global
            let
              defaultTex = texMap HM.! "assets/DEFAULT.png"
              dirt = 
                HM.lookupDefault defaultTex "assets/dirt.png" texMap
              grass = 
                HM.lookupDefault defaultTex "assets/grass.png" texMap
            case tile of
              0 -> pure ()
              1 -> TextureManager.draw grass Nothing destR'
              2 -> TextureManager.draw dirt  Nothing destR'
              _ -> pure ()
