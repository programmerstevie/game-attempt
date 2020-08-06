{-# LANGUAGE ScopedTypeVariables #-}
module Init where


import qualified Constants as Cons
import qualified TextureManager
import ECS.Base
import qualified Data.HashMap.Strict as HM


import Apecs
import Linear


initPlayer :: System' ()
initPlayer = do
  TextureManager.loadAnimationMap "assets\\animations\\ManAnimations.json"
  animationMap <- unAnimationMap <$> get global
  Time time <- get global
  player <- newEntity
              ( (Player, EntityName "player")
              , Stand
              , FaceRight
              , initPhysics
              , (animationMap HM.! "player.idle") { time0_A = time }
              , Active True
              )
  player $= ( WalkSpeed 13
            , WalkAccel 60
            , JumpSpeed Cons.playerJumpSpeed
            , JumpStrafe 60
            , TerminalVelocity (-40)
            , Position $ V2 0 7
            )
  player $= AABB  { center_aabb   = 0
                  , halfSize_aabb = V2 (3/8) (7/16)
                  , offset_aabb   = V2 (1/2) (7/16)
                  , scale_aabb    = V2 1 1
                  }
