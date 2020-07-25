{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{- HLINT ignore "Redundant do" -}
module ECS.Manager where


import ECS.Base
import qualified Utils
import Utils ((^^*))
import qualified Constants as Cons
import qualified Camera
import qualified Renderer
import qualified Action


import Apecs
import qualified SDL
import qualified TextureManager
import qualified Data.HashMap.Strict as HM
import Data.Bits
import Control.Monad (unless, when)
import Linear



updateSprites :: System' ()
updateSprites = do
  Renderer.setCamViewPort

  cmapM $ \( Position pos, spr@Sprite { destRect_S = dest } ) ->
    case dest of
      Nothing -> pure spr
      Just (SDL.Rectangle _ (V2 _ h)) -> do
        destPos <- Camera.worldToSdlCoords $ pos + Utils.scaleToWorld (V2 0 h)
        pure spr{ destRect_S = Utils.setRectPos dest destPos }


actionUpdate :: System' ()
actionUpdate =
  cmapM_ $ \(action :: Action, _ :: PhysicsComponents, ety :: Entity) -> do
    (  WalkSpeed wlkspd
     , TerminalVelocity termvel
     , CollisionFlags collisionFlags) <- get ety
    let onGround = testBit collisionFlags 10
    unless onGround $
      ety $= Jump
    case action of
      Stand -> do
        -- Utils.consoleLog "Standing"
        -- do animation
        Action.correctXVel ety 0 Cons.friction
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
  Renderer.setCamViewPort
  cmapM_ $ \Sprite{ 
             filePath_S = path
           , srcRect_S  = src
           , destRect_S = dest
           , flip_S     = flp
           } -> do
    let 
      defaultTex = texMap HM.! "assets/DEFAULT.png"
      tex = HM.lookupDefault defaultTex path texMap
    TextureManager.draw tex src dest flp
  cmapM_ $ \(aabb :: AABB) -> drawAABB aabb


drawAABB :: AABB -> System' ()
drawAABB AABB{ center   = ctr
             , halfSize = hs 
             , scale    = scl } = do
  Renderer.setCamViewPort
  rectCoords <- Camera.worldToSdlCoords $ ctr + Utils.modX negate (scl ^^* hs)
  rectSize   <- Camera.scaleToCamera $ 
                  fmap floor $ (scl ^^* hs) ^* (2 * Cons.coordsScale)
  let rect = Utils.makeRect rectCoords rectSize
  CRenderer renderer <- get global
  SDL.rendererDrawColor renderer SDL.$= V4 200 0 0 255
  SDL.drawRect renderer (Just rect)


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