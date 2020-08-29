{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf          #-}
{- HLINT ignore "Redundant do" -}
module ECS.Manager where


import ECS.Base
import qualified Utils
import qualified ECS.AnimationManager as Anim
import Utils (($>>))
import qualified Constants as Cons
import qualified DinoBehavior
import qualified Player
import qualified Action


import Apecs
import Data.Bits
import Control.Monad (unless, when)
import Linear


npcUpdate :: System' ()
npcUpdate = do
  DinoBehavior.updateDinos
  Player.updatePlayer


actionUpdate :: System' ()
actionUpdate =
  cmapM_ $ \(action :: Action, _ :: PhysicsComponents, ety :: Entity) -> do
    (  WalkSpeed wlkspd
     , TerminalVelocity termvel
     , CollisionFlags collisionFlags ) <- get ety
    let onGround = testBit collisionFlags 10
    unless onGround $
      ety $= Jump
    case action of
      Stand -> do
        -- Utils.consoleLog "Standing"
        Anim.runAnimation ety "idle"
        Action.correctXVel ety 0 Cons.friction
      Walk -> do
        -- Utils.consoleLog "Walking"
        Anim.runAnimation ety "walk"
        ety $~ \(Velocity (V2 vx vy)) -> 
          Velocity (V2 (signum vx * min (abs vx) wlkspd) vy)
        ety $>> \(Velocity (V2 vx _)) -> do
          when (vx > 0) $
            ety $= FaceRight
          when (vx < 0) $
            ety $= FaceLeft
      Jump -> do
        -- Utils.consoleLog "Jump"
        ety $>> \(Velocity (V2 _ vy)) ->
          if vy >= 0
          then Anim.runAnimation ety "jump_up"
          else Anim.runAnimation ety "jump_down"
        ety $~ \(Velocity (V2 vx vy)) ->
          Velocity $ V2 (signum vx * min (abs vx) wlkspd) (max termvel vy)
        when onGround $ do
          ety $= FastFalling False
          ety $>> \(Velocity (V2 vx _)) -> do
            ety $= if vx == 0 then Stand else Walk
            when (vx > 0) $
              ety $= FaceRight
            when (vx < 0) $
              ety $= FaceLeft


refresh :: System' ()
refresh = cmapM_ $
  \(Active active, e :: Entity) ->
  unless active $ do
    Utils.consoleLog "Deleted entity!"
    destroy e (Proxy :: Proxy EntityComponents)




{-
drawTrail :: System' ()
drawTrail = do
  (PixelTrail trail, Renderer renderer) <- get global
  SDL.rendererDrawColor renderer SDL.$= V4 255 0 0 255
  SDL.drawPoints renderer (Vector.fromList $ take 100 trail)


updateTrail :: System' ()
updateTrail =
  cmapM_ $ \(Player, Position pos, Velocity vel) -> do
    let V2 px py = Utils.worldToSdlCoords pos
    global $~ \(PixelTrail trail) -> PixelTrail (SDL.P (V2 px py) : trail)
-}