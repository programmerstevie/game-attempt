{- HLINT Ignore "Reduce duplication." -}
module Collisions where

import ECS.Base
import qualified Utils
import Utils ((^^*))
import qualified Constants
import qualified ECS.TileMapComponent as TileMap 

import Data.Foldable
import Foreign.C.Types (CFloat)
import Linear


{-
returns the X to be snapped to after a right wall collision
-}
rightWallCollision :: MapTiles 
                   -> AABB
                   -> OldPosition
                   -> Position
                   -> Maybe CFloat
rightWallCollision map_m
                   aabb
                   (Old (Position oldPos))
                   (Position pos)
    = asum . map loopX $ tileIndexXs
  where
    hs   = halfSize_aabb aabb ^^* scale_aabb aabb
    ofst = offset_aabb   aabb ^^* scale_aabb aabb

    ctr = pos + ofst
    oldCtr = oldPos + ofst

    oldBottomRight = Utils.roundVec $
      oldCtr + Utils.modY negate hs + V2 Constants.onePix 0
    newBottomRight = Utils.roundVec $
      ctr + Utils.modY negate hs + V2 Constants.onePix 0
    --newTopRight = Utils.roundVec $ 
    --  newBottomRight + V2 0 (Utils.getY hs * 2)

    endX = Utils.worldToMapX map_m $ Utils.getX newBottomRight
    begX = min endX $ Utils.worldToMapX map_m (Utils.getX oldBottomRight) + 1
    dist = max 1 $ abs (endX - begX)

    tileIndexXs = takeWhile (<= endX) [begX, begX + 1..]

    loopX tileIndexX = loopY checkedTiles
      where 
        bottomRight = Utils.lerp
          newBottomRight
          oldBottomRight
          $ abs (fromIntegral $ endX - tileIndexX) / fromIntegral dist
        topRight = bottomRight + V2 0 (Utils.getY hs * 2)

        checkedTiles = iterate (+ V2 0 1) bottomRight
        
        loopY [] = Nothing
        loopY (checkedTile:tilesToCheck)
          | obstacle && not obstacleLeft = Just wallX
          | checkedTileY >= Utils.getY topRight = Nothing
          | otherwise = loopY tilesToCheck
            where
              checkedTileY = min (Utils.getY checkedTile) (Utils.getY topRight)
              tileIndexY = Utils.worldToMapY map_m checkedTileY
              wallX = Utils.mapToWorldX map_m tileIndexX
              obstacle = TileMap.isObstacle map_m $ V2 tileIndexY tileIndexX

              leftTileIndex = V2 tileIndexY (tileIndexX - 1)
              obstacleLeft = TileMap.isObstacle map_m leftTileIndex
                          || ( isOneWayPlatform map_m leftTileIndex
                            && (Utils.getY (oldCtr - ctr) > 0)
                            && (Utils.getY oldBottomRight > checkedTileY)
                             )


{-
returns the X to be snapped to after a left wall collision
-}
leftWallCollision :: MapTiles
                  -> AABB
                  -> OldPosition
                  -> Position
                  -> Maybe CFloat
leftWallCollision map_m
                  aabb
                  (Old (Position oldPos))
                  (Position pos)
    = asum . map loopX $ tileIndexXs
  where
    hs   = halfSize_aabb aabb ^^* scale_aabb aabb
    ofst = offset_aabb   aabb ^^* scale_aabb aabb

    ctr = pos + ofst
    oldCtr = oldPos + ofst

    oldBottomLeft = Utils.roundVec $
      oldCtr - hs - V2 Constants.onePix 0
    newBottomLeft = Utils.roundVec $
      ctr - hs - V2 Constants.onePix 0
    --newTopLeft = Utils.roundVec $
    --  newBottomLeft + V2 0 (Utils.getY hs * 2)

    endX = Utils.worldToMapX map_m $ Utils.getX newBottomLeft
    begX = max endX $ Utils.worldToMapX map_m (Utils.getX oldBottomLeft) - 1
    dist = max 1 $ abs (endX - begX)

    tileIndexXs = takeWhile (>= endX) [begX, begX - 1..]

    loopX tileIndexX = loopY checkedTiles
      where 
        bottomLeft = Utils.lerp
          newBottomLeft
          oldBottomLeft
          $ abs (fromIntegral $ endX - tileIndexX) / fromIntegral dist
        topLeft = bottomLeft + V2 0 (Utils.getY hs * 2)

        checkedTiles = iterate (+ V2 0 1) bottomLeft
        
        loopY [] = Nothing
        loopY (checkedTile:tilesToCheck)
          | obstacle && not obstacleRight = Just wallX
          | checkedTileY >= Utils.getY topLeft = Nothing
          | otherwise = loopY tilesToCheck
            where
              checkedTileY = min
                (Utils.getY checkedTile)
                (Utils.getY topLeft)
              tileIndexY = Utils.worldToMapY map_m checkedTileY
              wallX = Utils.mapToWorldX map_m tileIndexX + 1
              obstacle = TileMap.isObstacle map_m $ V2 tileIndexY tileIndexX

              rightTileIndex = V2 tileIndexY (tileIndexX + 1)
              obstacleRight = TileMap.isObstacle map_m rightTileIndex
                           || ( isOneWayPlatform map_m rightTileIndex
                             && (Utils.getY (oldCtr - ctr) > 0)
                             && (Utils.getY oldBottomLeft > checkedTileY)
                              )



{-
returns the y to snapped to after a ceiling collision
-}
ceilingCollision :: MapTiles 
                 -> AABB
                 -> OldPosition
                 -> Position
                 -> Maybe CFloat 
ceilingCollision map_m 
                 aabb
                 (Old (Position oldPos))
                 (Position pos) 
    = asum . map loopY $ tileIndexYs
  where
    hs   = halfSize_aabb aabb ^^* scale_aabb aabb
    ofst = offset_aabb   aabb ^^* scale_aabb aabb

    ctr    = pos + ofst
    oldCtr = oldPos + ofst

    oldTopRight = Utils.roundVec $
      oldCtr + hs + V2 (-Constants.onePix) Constants.onePix
    newTopRight = Utils.roundVec $
      ctr + hs + V2 (-Constants.onePix) Constants.onePix
    --newTopLeft = Utils.roundVec $
    --  newTopRight + V2 (2 * Constants.onePix - Utils.getX hs * 2) 0

    endY = Utils.worldToMapY map_m $ Utils.getY newTopRight
    begY = max endY $ Utils.worldToMapY map_m (Utils.getY oldTopRight) - 1
    dist = max 1 $ abs (endY - begY)

    tileIndexYs = takeWhile (>= endY) [begY, begY - 1..]

    loopY tileIndexY = loopX checkedTiles
      where
        topRight = Utils.lerp
          newTopRight
          oldTopRight
          $ abs (fromIntegral $ endY - tileIndexY) / fromIntegral dist
        topLeft = topRight + V2 (2 * Constants.onePix - Utils.getX hs * 2) 0
        
        checkedTiles = iterate (+ V2 1 0) topLeft

        loopX [] = Nothing
        loopX (checkedTile:tilesToCheck)
          | obstacle && not obstacleUnder = Just ceilingY
          | checkedTileX >= Utils.getX topRight = Nothing
          | otherwise = loopX tilesToCheck
            where
              checkedTileX = min
                (Utils.getX checkedTile)
                (Utils.getX topRight)
              tileIndexX = Utils.worldToMapX map_m checkedTileX
              ceilingY = Utils.mapToWorldY map_m tileIndexY
              obstacle = TileMap.isObstacle map_m $ V2 tileIndexY tileIndexX

              obstacleUnder = TileMap.isObstacle map_m $ V2 (tileIndexY + 1) tileIndexX



{- 
returns the Y to be snapped to after a ground collision
and whether it's a one way platform 
-}
groundCollision :: MapTiles 
                 -> AABB
                 -> OldPosition
                 -> Position
                 -> Maybe (CFloat, Bool) 
groundCollision map_m 
                aabb
                (Old (Position oldPos))
                (Position pos)
  = asum . map loopY $ tileIndexYs
  where
    hs   = halfSize_aabb aabb ^^* scale_aabb aabb
    ofst = offset_aabb   aabb ^^* scale_aabb aabb

    oldCtr = oldPos + ofst
    ctr    = pos + ofst

    oldBottomLeft = Utils.roundVec $
      oldCtr - hs + V2 Constants.onePix (-Constants.onePix)
    newBottomLeft = Utils.roundVec $
      ctr - hs + V2 Constants.onePix (-Constants.onePix)
    --newBottomRight = Utils.roundVec $
    --  newBottomLeft + V2 (Utils.getX hs * 2 - 2 * Constants.onePix) 0

    endY = Utils.worldToMapY map_m $ Utils.getY newBottomLeft
    begY = min endY $ Utils.worldToMapY map_m (Utils.getY oldBottomLeft) + 1
    dist = max 1 $ abs (endY - begY)


    tileIndexYs = takeWhile (<= endY) [begY, begY + 1..]

    loopY tileIndexY = loopX checkedTiles
      where
        bottomLeft = Utils.lerp
          newBottomLeft
          oldBottomLeft
          $ abs (fromIntegral $ endY - tileIndexY) / fromIntegral dist
        bottomRight = bottomLeft
                    + V2 (Utils.getX hs * 2 - 2 * Constants.onePix) 0

        checkedTiles = iterate (+ V2 1 0) bottomLeft
        
        loopX [] = Nothing
        loopX (checkedTile:tilesToCheck)
          | obstacle && not obstacleOnTop = Just (groundY, False)
          | checkedTileX >= Utils.getX bottomRight = 
              if onOneWay
              then Just (groundY, True)
              else Nothing
          | otherwise = loopX tilesToCheck
            where
              checkedTileX = min
                (Utils.getX checkedTile)
                (Utils.getX bottomRight)
              tileIndexX = Utils.worldToMapX map_m checkedTileX
              groundY = Utils.mapToWorldY map_m tileIndexY + 1
              tileIndex = V2 tileIndexY tileIndexX
              obstacle = TileMap.isObstacle map_m tileIndex
              obstacleOnTop = TileMap.isObstacle map_m $ V2 (tileIndexY - 1) tileIndexX
              
              onOneWay = TileMap.isOneWayPlatform map_m tileIndex &&
                ( abs (Utils.getY checkedTile - groundY) 
                <= 
                  ( Constants.oneWayPlatformThreshold 
                  + Utils.getY oldPos - Utils.getY pos ) )



overlaps :: AABB -> AABB -> Bool
overlaps AABB{ center_aabb   = V2 cxA cyA
             , halfSize_aabb = V2 hWA hHA }
         AABB{ center_aabb   = V2 cxB cyB
             , halfSize_aabb = V2 hWB hHB }
  =  (abs (cxA - cxB) <= hWA + hWB) 
  && (abs (cyA - cyB) <= hHA + hHB)