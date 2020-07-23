{- HLINT Ignore "Reduce duplication." -}
module Collisions where

import ECS.Base
import qualified Utils
import Utils ((>*<))
import qualified Constants


import Apecs
import qualified Data.Array as Array
import Control.Monad
import Data.Maybe (fromJust)
import Data.Foldable
import Foreign.C.Types (CInt, CFloat)
import Linear
import Debug.Trace


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
    hs   = halfSize aabb >*< scale aabb
    ofst = offset   aabb >*< scale aabb

    ctr = pos + ofst
    oldCtr = oldPos + ofst

    oldBottomRight = Utils.roundVec $
      oldCtr + Utils.modY negate hs + V2 Constants.onePix 0
    newBottomRight = Utils.roundVec $
      ctr + Utils.modY negate hs + V2 Constants.onePix 0
    newTopRight = Utils.roundVec $
      newBottomRight + V2 0 (Utils.getY hs * 2)

    endX = Utils.worldXToMapX $ Utils.getX newBottomRight
    begX = min endX $ Utils.worldXToMapX (Utils.getX oldBottomRight) + 1
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
              tileIndexY = Utils.worldYToMapY checkedTileY
              wallX = Utils.mapXToWorldX tileIndexX
              obstacle = isObstacle map_m $ V2 tileIndexY tileIndexX

              leftTileIndex = V2 tileIndexY (tileIndexX - 1)
              obstacleLeft = isObstacle map_m leftTileIndex
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
    hs   = halfSize aabb >*< scale aabb
    ofst = offset   aabb >*< scale aabb

    ctr = pos + ofst
    oldCtr = oldPos + ofst

    oldBottomLeft = Utils.roundVec $
      oldCtr - hs - V2 Constants.onePix 0
    newBottomLeft = Utils.roundVec $
      ctr - hs - V2 Constants.onePix 0
    newTopLeft = Utils.roundVec $
      newBottomLeft + V2 0 (Utils.getY hs * 2)

    endX = Utils.worldXToMapX $ Utils.getX newBottomLeft
    begX = max endX $ Utils.worldXToMapX (Utils.getX oldBottomLeft) - 1
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
              tileIndexY = Utils.worldYToMapY checkedTileY
              wallX = Utils.mapXToWorldX tileIndexX + 1
              obstacle = isObstacle map_m $ V2 tileIndexY tileIndexX

              rightTileIndex = V2 tileIndexY (tileIndexX + 1)
              obstacleRight = isObstacle map_m rightTileIndex
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
    hs   = halfSize aabb >*< scale aabb
    ofst = offset   aabb >*< scale aabb

    ctr    = pos + ofst
    oldCtr = oldPos + ofst

    oldTopRight = Utils.roundVec $
      oldCtr + hs + V2 (-Constants.onePix) Constants.onePix
    newTopRight = Utils.roundVec $
      ctr + hs + V2 (-Constants.onePix) Constants.onePix
    newTopLeft = Utils.roundVec $
      newTopRight + V2 (2 * Constants.onePix - Utils.getX hs * 2) 0

    endY = Utils.worldYToMapY $ Utils.getY newTopRight
    begY = max endY $ Utils.worldYToMapY (Utils.getY oldTopRight) - 1
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
              tileIndexX = Utils.worldXToMapX checkedTileX
              ceilingY = Utils.mapYToWorldY tileIndexY
              obstacle = isObstacle map_m $ V2 tileIndexY tileIndexX

              obstacleUnder = isObstacle map_m $ V2 (tileIndexY + 1) tileIndexX



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
    hs   = halfSize aabb >*< scale aabb
    ofst = offset   aabb >*< scale aabb

    oldCtr = oldPos + ofst
    ctr    = pos + ofst

    oldBottomLeft = Utils.roundVec $
      oldCtr - hs + V2 Constants.onePix (-Constants.onePix)
    newBottomLeft = Utils.roundVec $
      ctr - hs + V2 Constants.onePix (-Constants.onePix)
    newBottomRight = Utils.roundVec $
      newBottomLeft + V2 (Utils.getX hs * 2 - 2 * Constants.onePix) 0

    endY = Utils.worldYToMapY $ Utils.getY newBottomLeft
    begY = min endY $ Utils.worldYToMapY (Utils.getY oldBottomLeft) + 1
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
              tileIndexX = Utils.worldXToMapX checkedTileX
              groundY = Utils.mapYToWorldY tileIndexY + 1
              tileIndex = V2 tileIndexY tileIndexX
              obstacle = isObstacle map_m tileIndex
              obstacleOnTop = isObstacle map_m $ V2 (tileIndexY - 1) tileIndexX
              
              onOneWay = isOneWayPlatform map_m tileIndex &&
                ( abs (Utils.getY checkedTile - groundY) 
                <= 
                  ( Constants.oneWayPlatformThreshold 
                  + Utils.getY oldPos - Utils.getY pos ) )


isOneWayPlatform :: MapTiles -> V2 CInt -> Bool
isOneWayPlatform map_m coords
  | Array.inRange (Array.bounds map_m) coords =
      case map_m Array.! coords of
        3 -> True
        _ -> False
  | otherwise = False


isObstacle :: MapTiles -> V2 CInt -> Bool
isObstacle map_m coords
  | Array.inRange (Array.bounds map_m) coords =
      case map_m Array.! coords of
        1 -> True
        2 -> True
        _ -> False
  | otherwise = True



overlaps :: AABB -> AABB -> Bool
overlaps AABB{ center   = V2 cxA cyA
             , halfSize = V2 hWA hHA }
         AABB{ center   = V2 cxB cyB
             , halfSize = V2 hWB hHB }
  =  (abs (cxA - cxB) <= hWA + hWB) 
  && (abs (cyA - cyB) <= hHA + hHB)