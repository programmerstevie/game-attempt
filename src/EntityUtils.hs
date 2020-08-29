{-# LANGUAGE ScopedTypeVariables #-}
module EntityUtils where

import ECS.Base
import qualified ECS.TileMapComponent as TileMap
import qualified Constants
import qualified Utils

import Apecs
import Linear
import Data.Bits
import Foreign.C.Types (CFloat)


unfreeze :: Entity -> System' ()
unfreeze ety = destroy ety (Proxy :: Proxy Wait)


freeze :: Entity -> CFloat -> System' ()
freeze ety frozenTime = do
  Time t <- get global
  ety $= Wait (t + frozenTime)


turnAround :: Facing -> Facing
turnAround FaceLeft  = FaceRight
turnAround FaceRight = FaceLeft


atRightEdge :: Entity -> System' Bool
atRightEdge ety = do
  (aabb :: AABB, CollisionFlags flags) <- get ety
  let ctr = center_aabb aabb
      hs  = halfSize_aabb aabb Utils.^^* scale_aabb aabb
      bottomRight = Utils.roundVec $
                     ctr + Utils.modY negate hs 
                         + V2 Constants.onePix (-Constants.onePix)
  if not $ testBit flags 10
  then pure False
  else do
    map_m <- map_M <$> get global
    let tile = Utils.worldToMapCoords map_m bottomRight
    pure . not $ TileMap.isObstacle map_m tile
        || TileMap.isOneWayPlatform map_m tile


atLeftEdge :: Entity -> System' Bool
atLeftEdge ety = do
  (aabb :: AABB, CollisionFlags flags) <- get ety
  let ctr = center_aabb aabb
      hs  = halfSize_aabb aabb Utils.^^* scale_aabb aabb
      bottomLeft = Utils.roundVec $
                    ctr - hs + V2 (-Constants.onePix) (-Constants.onePix)
  if not $ testBit flags 10
  then pure False
  else do
    map_m <- map_M <$> get global
    let tile = Utils.worldToMapCoords map_m bottomLeft
    pure . not $ TileMap.isObstacle map_m tile
        || TileMap.isOneWayPlatform map_m tile