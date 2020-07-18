{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
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


update :: System' ()
update = do
  (DT dT, Gravity g) <- get global
  updateOld
  cmapM_ $ \(_ :: PhysicsComponents, ety :: Entity) -> do
    ety $>> \(Position pos, Velocity vel) -> 
      Utils.setPos ety (Position $ pos + vel ^* dT)
    ety $~  \(Velocity vel) -> Velocity (vel + g ^* dT)

    ety $>> \(Velocity vel, Position pos) -> do
      groundCollision <- Collisions.groundCollision ety
      if Utils.getY vel <= 0 && isJust groundCollision
        then do
          let groundY = fromJust groundCollision
          Utils.setPos ety (Position $ Utils.setY pos groundY)
          ety $= ( Velocity $ Utils.setY vel 0
                 , OnGround True
                 )
        else
          ety $= OnGround False
    --OnGround onground' <- get e
    --Utils.conShowLog onground'


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


