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
--import qualified ECS.KeyboardController as Key
import qualified SDL
import qualified TextureManager
import qualified Data.HashMap.Strict as HM
import Foreign.C.Types (CFloat)
import Control.Monad (unless, when)
import Linear


update :: System' ()
update = do
  refresh
  cmap $ \( Position pos, spr@Sprite { destRect_S = dest } ) ->
    case dest of
      Nothing -> spr
      Just (SDL.Rectangle _ (V2 _ h)) ->
        spr{ destRect_S = Utils.setRectPos dest $
              Utils.worldToSdlCoords (pos + Utils.scaleToWorld (V2 0 h)) }
  actionUpdate


actionUpdate :: System' ()
actionUpdate =
  cmapM_ $ \(action :: Action, _ :: PhysicsComponents, ety :: Entity) -> do
    (  JumpSpeed jmpspd
     , WalkSpeed wlkspd
     , TerminalVelocity termvel
     , OnGround onGround
     , PushesRightWall pushesRightWall
     , PushesLeftWall pushesLeftWall   ) <- get ety
    DT dT <- get global
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
        Action.correctXVel ety wlkspd Constants.correctWalkFriction
      Jump -> do
        -- Utils.consoleLog "Jump"
        -- do animation based on up or down
        Action.correctXVel ety wlkspd Constants.airFriction
        ety $~ \(Velocity vel) -> Velocity $ Utils.modY (max termvel) vel
        when (pushesRightWall || pushesLeftWall) $
          ety $~ \(Velocity vel) -> Velocity $ Utils.setX vel 0
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