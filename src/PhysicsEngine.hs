{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{- HLINT ignore "Redundant do" -}
module PhysicsEngine where


import ECS.Base
import qualified Utils
import Utils (($~~), ($>>), (>*<))
import qualified Constants as Cons
import qualified Collisions

import Apecs
import qualified SDL
import Linear
import Foreign.C.Types (CInt)
import Data.Maybe (isJust, fromJust)
import Data.Foldable (asum)
import Control.Monad
import Data.Bits


update :: System' ()
update = do
  DT dT <- get global
  updateOld
  cmapM_ $ \(_ :: PhysicsComponents, ety :: Entity) -> do
    ety $>> \(Position pos, Velocity vel) ->
      Utils.setPos ety (Position $ pos + vel ^* dT)
    ety $>> \(CollisionFlags coll, Velocity vel) -> do
      let onGround = testBit coll 10
      unless onGround $ ety $= Velocity (vel + V2 0 (Cons.gravity * dT))
    updateTileCollisions ety
    updateAABB ety


updateAABB :: Entity -> System' AABB
updateAABB ety = do
  ety $~~ \(Position pos, aabb@AABB{ offset = ofst, scale = scl }) ->
    aabb{ center = pos + (ofst >*< scl) }
  get ety


updateOld :: System' ()
updateOld = cmap $ 
   \( Position pos
    , Velocity vel
    , CollisionFlags coll
    ) ->
    ( Old $ Position pos
    , Old $ Velocity vel
    , Old $ CollisionFlags coll
    )


updateTileCollisions :: Entity -> System' ()
updateTileCollisions ety = do
  aabb_ :: AABB <- get ety
  let hs = halfSize aabb_ >*< scale aabb_
      ofst = offset aabb_ >*< scale aabb_
  mapTiles <- map_M <$> get global

  -- when to move entity to the right (collides with left wall)
  ety $>> \( Velocity vel
           , pos'@(Position pos)
           , oldpos'@(Old (Position oldpos))
           , aabb :: AABB
           , CollisionFlags coll
           ) -> do
    let 
      leftCollide = Collisions.leftWallCollision mapTiles aabb oldpos' pos'
      leftX = fromJust leftCollide
    if Utils.getX vel <= 0 && isJust leftCollide
    then do
      --Utils.consoleLog "Hit the Left Wall!"
      when (Utils.getX (oldpos - hs + ofst) >= leftX) $ do
        Utils.setPos ety $ 
          Position $ Utils.setX pos $ leftX + Utils.getX (hs - ofst)
        ety $= CollisionFlags (setBit coll 8)
      ety $= Velocity (Utils.modX (max 0) vel)
    else ety $= CollisionFlags (clearBit coll 8)

  -- when to move entity to the left (collides with right wall)
  ety $>> \( Velocity vel
           , pos'@(Position pos)
           , oldpos'@(Old (Position oldpos))
           , aabb :: AABB
           , CollisionFlags coll
           ) -> do
    let
      rghtCollide = Collisions.rightWallCollision mapTiles aabb oldpos' pos'
      rghtX = fromJust rghtCollide
    if Utils.getX vel >= 0 && isJust rghtCollide
    then do
      --Utils.consoleLog "Hit the Right Wall!"
      when (Utils.getX (oldpos + hs + ofst) <= rghtX) $ do
        Utils.setPos ety $ 
          Position $ Utils.setX pos $ rghtX - Utils.getX (hs + ofst)
        ety $= CollisionFlags (setBit coll 9)
      ety $= Velocity (Utils.modX (min 0) vel)
    else ety $= CollisionFlags (clearBit coll 9)
  
  -- when to move entity upwards (collides with ground)
  ety $>> \( Velocity vel
           , pos'@(Position pos)
           , oldpos'@(Old (Position oldpos))
           , aabb :: AABB
           , CollisionFlags coll
           ) -> do
    let
      grndCollide = Collisions.groundCollision mapTiles aabb oldpos' pos'
      (grndY, oneWay) = fromJust grndCollide
    if Utils.getY vel <= 0 && isJust grndCollide
    then do
      --Utils.consoleLog "Hit the Ground!"
      Utils.setPos ety $
        Position $ Utils.setY pos $ grndY + Utils.getY (hs - ofst)
      ety $= ( Velocity (Utils.setY vel 0)
             , CollisionFlags $ setBit coll 10
             , OnOneWayPlatform oneWay )
    else ety $= CollisionFlags (clearBit coll 10)
  
  -- when to move entity downwards (collides with ceiling)
  ety $>> \( Velocity vel
           , pos'@(Position pos)
           , oldpos'@(Old (Position oldpos))
           , aabb :: AABB
           , CollisionFlags coll
           ) -> do
    let 
      ceilCollide = Collisions.ceilingCollision mapTiles aabb oldpos' pos'
      ceilY = fromJust ceilCollide
    if Utils.getY vel >= 0 && isJust ceilCollide
    then do
      --Utils.consoleLog "Hit the Ceiling!"
      Utils.setPos ety $
        Position $ Utils.setY pos $ ceilY - Utils.getY hs * 2 - Cons.onePix
      ety $= ( Velocity (Utils.setY vel 0)
             , CollisionFlags $ setBit coll 11 )
    else ety $= CollisionFlags (clearBit coll 11)

