module Init where


import qualified Constants
import ECS.Base
import qualified ECS.SpriteComponent
import qualified Utils
import Utils (($~~))


import Apecs
import Linear


initPlayer :: System' ()
initPlayer = do
  player <- newEntity 
              ( Player
              , KeyboardControl
              , Stand
              , FaceRight
              , initPhysics
              , Active True
              )
  set player =<< ECS.SpriteComponent.init "assets\\player.png" 64 64 -- make setM
  player $= ( WalkSpeed 13
            , JumpAccel 60
            , WalkAccel 60
            , JumpHeight Constants.playerJumpHeight
            , TerminalVelocity (-40)
            , OnGround True
            , Position $ V2 0 7
            )
  player $~~ \(Position pos) ->
    AABB  { center   = 0
          , halfSize = V2 (3/4) (7/8)
          , offset   = V2 1 (7/8)
          }
  Utils.setJumpVel player
