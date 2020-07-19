{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{- HLINT ignore "Redundant do" -}
module PhysicsEngine where


import ECS.Base
import qualified Utils
import Utils (($~~), ($>>))
import qualified Constants as Cons
import qualified Collisions

import Apecs
import qualified SDL
import Linear
import Foreign.C.Types (CInt)
import Data.Maybe (isJust, fromJust)
import Data.Foldable (asum)
import Control.Monad


update :: System' ()
update = do
  (DT dT, Gravity g) <- get global
  updateOld
  cmapM_ $ \(_ :: PhysicsComponents, ety :: Entity) -> do
    ety $>> \(Position pos, Velocity vel) ->
      Utils.setPos ety (Position $ pos + vel ^* dT)
    ety $>> \(OnGround onGround, Velocity vel) ->
      unless onGround $ ety $= Velocity (vel + g ^* dT)
    updateAABB ety
    updateTileCollisions ety


updateAABB :: Entity -> System' AABB
updateAABB ety = do
  ety $~~ \(Position pos, aabb :: AABB) -> aabb{ center = pos + offset aabb }
  get ety


updateOld :: System' ()
updateOld = cmap $ 
   \( Position pos
    , Velocity vel
    , ( PushesRightWall right
      , PushesLeftWall left
      , OnGround grnd
      , AtCeiling ceil
      )
    ) ->
    ( Old $ Position pos
    , Old $ Velocity vel
    , ( Old $ PushesRightWall right
      , Old $ PushesLeftWall left
      , Old $ OnGround grnd
      , Old $ AtCeiling ceil
      )
    )


updateTileCollisions :: Entity -> System' ()
updateTileCollisions ety = do
  AABB{halfSize = hs, offset = ofst} <- get ety
  mapTiles <- fromJust . map_M <$> get global

  -- when to move entity to the right (collides with left wall)
  ety $>> \( Velocity vel
           , pos'@(Position pos)
           , oldpos'@(Old (Position oldpos))
           , aabb :: AABB
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
        ety $= PushesLeftWall True
        ety $= Velocity (Utils.modX (max 0) vel)
    else ety $= PushesLeftWall False

  -- when to move entity to the left (collides with right wall)
  ety $>> \( Velocity vel
           , pos'@(Position pos)
           , oldpos'@(Old (Position oldpos))
           , aabb :: AABB
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
        ety $= PushesRightWall True
        ety $= Velocity (Utils.modX (min 0) vel)
    else ety $= PushesRightWall False
  
  -- when to move entity upwards (collides with ground)
  ety $>> \( Velocity vel
           , pos'@(Position pos)
           , oldpos'@(Old (Position oldpos))
           , aabb :: AABB
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
             , OnGround True
             , OnOneWayPlatform oneWay )
    else ety $= OnGround False
  
  -- when to move entity downwards (collides with ceiling)
  ety $>> \( Velocity vel
           , pos'@(Position pos)
           , oldpos'@(Old (Position oldpos))
           , aabb :: AABB
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
             , AtCeiling True )
    else ety $= AtCeiling False

