{-# LANGUAGE TypeFamilies #-}
module ECS.EntityComponents where

import ECS.PhysicsComponents

import Apecs.Core (Component, Storage)
import Apecs.Stores (Map, Unique)
import qualified SDL
import Linear
import Foreign.C.Types (CInt, CFloat)


data Action = Stand | Walk | Jump

instance Component Action where
  type Storage Action = Map Action



data Facing = FaceLeft | FaceRight

instance Component Facing where
  type Storage Facing = Map Facing



data Player = Player

instance Component Player where
  type Storage Player = Unique Player



data Dinosaur = Dinosaur

instance Component Dinosaur where
  type Storage Dinosaur = Map Dinosaur



data Sprite = Sprite { filePath_S :: FilePath
                     , srcRect_S  :: Maybe (SDL.Rectangle CInt)
                     , destRect_S :: Maybe (SDL.Rectangle CInt) }

instance Component Sprite where
  type Storage Sprite = Map Sprite



newtype Active = Active Bool

instance Component Active where
  type Storage Active = Map Active



data KeyboardControl = KeyboardControl

instance Component KeyboardControl where
  type Storage KeyboardControl = Map KeyboardControl



newtype OnOneWayPlatform = OnOneWayPlatform Bool

instance Component OnOneWayPlatform where
  type Storage OnOneWayPlatform = Map OnOneWayPlatform



-- Aliases

type EntityComponents = ( Player
                        , Dinosaur
                        , KeyboardControl
                        , ( PhysicsComponents
                          , Action
                          , Facing
                          , Sprite
                          , Active
                          )
                        )

type PhysicsComponents
  = ( AABB
    , ( Position
      , Velocity
      , PushesRightWall
      , PushesLeftWall
      , AtCeiling
      , OnGround
      , OnOneWayPlatform
      )
    , ( OldPosition
      , OldVelocity
      , PushedRightWall
      , PushedLeftWall
      , WasOnGround
      , WasAtCeiling
      )
    , ( JumpHeight
      , JumpSpeed
      , JumpAccel
      , WalkSpeed
      , WalkAccel
      , TerminalVelocity
      )
    )

initPhysics :: PhysicsComponents
initPhysics = 
    ( AABB 0 0 0
    , ( Position 0
      , Velocity 0
      , PushesRightWall False
      , PushesLeftWall False
      , AtCeiling False
      , OnGround False
      , OnOneWayPlatform False
      )
    , ( Old $ Position 0
      , Old $ Velocity 0
      , Old $ PushesRightWall False
      , Old $ PushesLeftWall False
      , Old $ OnGround False
      , Old $ AtCeiling False
      )
    , ( JumpHeight 0
      , JumpSpeed 0
      , JumpAccel 0
      , WalkSpeed 0
      , WalkAccel 0
      , TerminalVelocity 0
      )
    )