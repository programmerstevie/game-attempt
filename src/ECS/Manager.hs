{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf          #-}
{- HLINT ignore "Redundant do" -}
module ECS.Manager where


import ECS.Base
import qualified Utils
import Utils ((^^*), ($>>))
import qualified Constants as Cons
import qualified Camera
import qualified Renderer
import qualified Action


import Apecs
import qualified SDL
import qualified TextureManager
import qualified Data.HashMap.Strict as HM
import Data.Bits
import Control.Applicative (liftA2)
import Data.Bool
import Control.Monad (unless, when)
import Linear


updateAnimations :: System' ()
updateAnimations = do
  Time t <- get global
  cmapM $ \(ani :: Animation, ety :: Entity) -> do
    let frame = floor ((t - time0_A ani) / delta_A ani) `rem` length_A ani
    hasSprite <- exists ety (Proxy :: Proxy Sprite)
    flipped <- if hasSprite then flip_S <$> get ety else pure Cons.noFlip
    return $ Sprite{ filePath_S = filePath_A ani
                   , srcRect_S  = srcRects_A ani !! frame
                   , destRect_S = destRects_A ani !! frame
                   , flip_S     = flipped
                   }


runAnimation :: Entity -> String -> System' ()
runAnimation ety animName = ety $>> \(AnimationName entName) -> do
  animationMap <- unAnimationMap <$> get global
  let name = entName ++ '.' : animName
      fixedAnimation =
        animationMap HM.!
          if HM.member name animationMap
          then name
          else entName ++ ".default"
  alreadyPlaying <- exists ety (Proxy :: Proxy Animation) >>= 
    \hasAnimation ->
    if hasAnimation
    then (name_A fixedAnimation ==) . name_A <$> get ety
    else pure False
  unless alreadyPlaying $ do
    Time time    <- get global
    ety $= fixedAnimation{ time0_A = time }


updateSprites :: System' ()
updateSprites = do
  updateAnimations
  cmap $ \(spr@Sprite{flip_S = flp}, facing :: Facing) ->
    case facing of
      FaceLeft  -> spr{flip_S = liftA2 (||) Cons.flipX flp }
      FaceRight -> spr{flip_S = liftA2 ((&&) . not) Cons.flipX flp }
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
     , CollisionFlags collisionFlags ) <- get ety
    let onGround = testBit collisionFlags 10
    unless onGround $
      ety $= Jump
    case action of
      Stand -> do
        -- Utils.consoleLog "Standing"
        runAnimation ety "idle"
        Action.correctXVel ety 0 Cons.friction
      Walk -> do
        -- Utils.consoleLog "Walking"
        runAnimation ety "walk"
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
          then runAnimation ety "jump_up"
          else runAnimation ety "jump_down"
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


draw :: System' ()
draw = do
  cmapM_ $ \( Sprite{
                filePath_S = path
              , srcRect_S  = src
              , destRect_S = dest
              , flip_S     = flp
             }
            , AABB{center_aabb = ctr}
            ) -> do
    viewport   <- Renderer.getViewPortSize

    scaledDest <- Camera.scaleRecToCamera dest
    aabbCtr    <- Camera.worldToSdlCoords ctr
    tex        <- TextureManager.getTexture path

    let
      destCtr = case scaledDest of
        Just (SDL.Rectangle (SDL.P pt) sz) -> pt + ((`div` 2) <$> sz)
        Nothing -> (`div` 2) <$> viewport

      scaledDest' = 
        Utils.modRectPos
          ((+) $ liftA2 (bool 0) (-2*^(destCtr - aabbCtr)) flp) 
          scaledDest
    TextureManager.draw tex src scaledDest' flp
  cmapM_ $ \(aabb :: AABB) -> drawAABB aabb


drawAABB :: AABB -> System' ()
drawAABB AABB{ center_aabb   = ctr
             , halfSize_aabb = hs 
             , scale_aabb    = scl } = do
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
    global $~ \(PixelTrail trail) -> PixelTrail (SDL.P (V2 px py) : trail)
-}