module Collisions where

import ECS.Base
import qualified Utils
import qualified Constants as Cons


import Apecs
import qualified Data.Array as Array
import Control.Monad
import Data.Maybe (fromJust)
import Data.Foldable
import Foreign.C.Types (CInt, CFloat)
import Linear



groundCollision :: Entity -> System' (Maybe CFloat)
groundCollision ety = do
  aabb@AABB{ halfSize = hs } <- get ety
  map_m <- fromJust . map_M <$> get global
  
  oldCtr <- (\(Old (Position oldPos)) -> oldPos + offset aabb) <$> get ety
  ctr    <- (\(Position pos) -> pos + offset aabb) <$> get ety

  let
    oldBottomLeft = oldCtr - hs + V2 Cons.onePix (-Cons.onePix)
    newBottomLeft =    ctr - hs + V2 Cons.onePix (-Cons.onePix)
    newBottomRight = 
      V2 ( Utils.getX newBottomLeft  + Utils.getX hs * 2 - 2 * Cons.onePix )
         ( Utils.getY newBottomLeft )

    endY = Utils.worldYToMapY $ Utils.getY newBottomLeft
    begY = max endY $ Utils.worldYToMapY (Utils.getY oldBottomLeft) - 1
    dist = max 1 $ abs (endY - begY)

  let
    tileIndexYs = takeWhile (>= endY) [begY, begY - 1..]

    loopY tileIndexY = loopX checkedTiles
      where 
        checkedTiles = iterate (Utils.modX (+1)) bottomLeft

        bottomLeft  = Utils.lerp
          newBottomLeft
          oldBottomLeft
          $ abs (fromIntegral $ endY - tileIndexY) / fromIntegral dist
        bottomRight = 
          Utils.modX (+ (Utils.getX hs * 2 - 2 * Cons.onePix)) bottomLeft
        
        loopX [] = Nothing
        loopX (checkedTile:tilesToCheck)
          | isObstacle map_m $ V2 tileIndexY tileIndexX = Just groundY
          | checkedTileX >= Utils.getX bottomRight = Nothing
          | otherwise = loopX tilesToCheck
            where
              checkedTileX = min 
                (Utils.getX checkedTile)
                (Utils.getX bottomRight)
              tileIndexX = Utils.worldXToMapX checkedTileX
              groundY = Utils.mapYToWorldY tileIndexY + 1
  
  return . asum . map loopY $ tileIndexYs




isObstacle :: MapTiles -> V2 CInt -> Bool
isObstacle map_m coords
  | Array.inRange (Array.bounds map_m) coords =
      case map_m Array.! coords of
        1 -> True
        2 -> True
        _ -> False
  | otherwise = False



overlaps :: AABB -> AABB -> Bool
overlaps AABB{ center   = V2 cxA cyA
             , halfSize = V2 hWA hHA }
         AABB{ center   = V2 cxB cyB
             , halfSize = V2 hWB hHB }
  =  (abs (cxA - cxB) <= hWA + hWB) 
  && (abs (cyA - cyB) <= hHA + hHB)