{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
--{-# LANGUAGE LambdaCase #-}
{- HLINT ignore "Redundant do" -}
module ECS.Manager where

import ECS.Base
import qualified Utils
import Utils (($~~), ($>=), ($>>))
import qualified Constants
import qualified Action

import Apecs
import qualified SDL
import qualified TextureManager
import qualified Data.HashMap.Strict as HM
import Data.Bits
import Foreign.C.Types (CFloat)
import Control.Monad (unless, when, forM_)
import Linear
import qualified Data.Vector.Storable as Vector


updateSprites :: System' ()
updateSprites =
  cmap $ \( Position pos, spr@Sprite { destRect_S = dest } ) ->
    case dest of
      Nothing -> spr
      Just (SDL.Rectangle _ (V2 _ h)) ->
        spr{ destRect_S = Utils.setRectPos dest $
          Utils.worldToSdlCoords (pos + Utils.scaleToWorld (V2 0 h)) }


actionUpdate :: System' ()
actionUpdate =
  cmapM_ $ \(action :: Action, _ :: PhysicsComponents, ety :: Entity) -> do
    (  JumpSpeed jmpspd
     , WalkSpeed wlkspd
     , TerminalVelocity termvel
     , CollisionFlags collisionFlags) <- get ety
    DT dT <- get global
    let onGround = testBit collisionFlags 10
    unless onGround $
      ety $= Jump
    case action of
      Stand -> do
        -- Utils.consoleLog "Standing"
        -- do animation
        Action.correctXVel ety 0 Constants.friction
      Walk -> do
        -- Utils.consoleLog "Walking"
        -- do animation
        ety $~ \(Velocity (V2 vx vy)) -> 
          Velocity $ V2 (signum vx * min (abs vx) wlkspd) vy
      Jump -> do
        -- Utils.consoleLog "Jump"
        -- do animation based on up or down
        ety $~ \(Velocity (V2 vx vy)) ->
          Velocity $ V2 (signum vx * min (abs vx) wlkspd) (max termvel vy)
        when onGround $
          ety $= Stand
      

draw :: System' ()
draw = do
  Textures texMap <- get global
  cmapM_ $ \Sprite{ 
             filePath_S = path
           , srcRect_S  = src
           , destRect_S = dest
           } -> do
    let 
      defaultTex = texMap HM.! "assets/DEFAULT.png"
      tex = HM.lookupDefault defaultTex path texMap
    TextureManager.draw tex src dest
  cmapM_ $ \(aabb :: AABB) -> Utils.drawAABB aabb


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
    global $~ \(PixelTrail trail) -> PixelTrail (SDL.P (V2 px py) : trail) -}