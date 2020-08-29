{-# LANGUAGE ScopedTypeVariables #-}
module Init where


import ECS.Base
import qualified Constants as Cons
import qualified TextureManager
import qualified Utils
import qualified PhysicsEngine



import qualified Data.HashMap.Strict as HM
import Apecs
import Linear


initPlayer :: Position -> System' ()
initPlayer position = do
  TextureManager.loadAnimationMap
    "assets\\animations\\Man\\ManAnimations.json"
  AnimationMap animationMap <- get global
  Time time <- get global
  player <- newEntity
              ( (Active True, Player, AnimationName "man")
              , Stand
              , FaceRight
              , (animationMap HM.! "man.idle") { time0_A = time }
              , initPhysics
              )
  player $= ( WalkSpeed 13
            , WalkAccel 60
            , JumpSpeed Cons.playerJumpSpeed
            , JumpStrafe 60
            , TerminalVelocity (-40)
            , position
            )
  player $= AABB  { center_aabb   = 0
                  , halfSize_aabb = V2 (4 / 16) (7 / 16)
                  , offset_aabb   = V2 (7 / 16) (7 / 16)
                  , scale_aabb    = V2 1 1
                  }
  PhysicsEngine.updateAABB player

initDino :: Position -> System' ()
initDino position = do
  Utils.consoleLog "made a dino!"
  TextureManager.loadAnimationMap 
    "assets\\animations\\Dino\\DinoAnimations.json"
  AnimationMap animationMap <- get global
  Time time <- get global
  dinosaur <- newEntity
            ( (Active True, Dinosaur, AnimationName "dino")
            , Stand
            , FaceRight
            , (animationMap HM.! "dino.idle") { time0_A = time }
            , initPhysics
            )
  dinosaur $= ( WalkSpeed 5
              , WalkAccel 60
              , TerminalVelocity (-40)
              , position
              )
  dinosaur $= AABB  { center_aabb   = 0
                    , halfSize_aabb = V2 ( 8 / 24) (12 / 24)
                    , offset_aabb   = V2 (16 / 24) (16 / 24)
                    , scale_aabb    = V2 1 1
                    }
  PhysicsEngine.updateAABB dinosaur
