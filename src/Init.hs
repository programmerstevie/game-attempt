{-# LANGUAGE ScopedTypeVariables #-}
module Init where


import qualified Constants as Cons
import ECS.Base
import qualified ECS.SpriteComponent


import Apecs
import Linear


initPlayer :: System' ()
initPlayer = do
  player <- newEntity
              ( Player
              , Stand
              , FaceRight
              , initPhysics
              , Active True
              )
  set player =<< ECS.SpriteComponent.init "assets\\player.png" 32 32
  player $= ( WalkSpeed 13
            , WalkAccel 60
            , JumpSpeed Cons.playerJumpSpeed
            , JumpStrafe 60
            , TerminalVelocity (-40)
            , Position $ V2 0 7
            )
  player $= AABB  { center   = 0
                  , halfSize = V2 (3/8) (7/16)
                  , offset   = V2 (1/2) (7/16)
                  , scale    = V2 1 1
                  }
  player $~ \(sprite :: Sprite) -> sprite{ flip_S = Cons.flipX }
