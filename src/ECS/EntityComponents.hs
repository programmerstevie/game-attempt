{-# LANGUAGE TypeFamilies #-}
module ECS.EntityComponents where

import ECS.PhysicsComponents

import Apecs.Core (Component, Storage)
import Apecs.Stores (Map, Unique)
import Foreign.C.Types (CFloat)


newtype AnimationName = AnimationName { unEntityName :: String }

instance Component AnimationName where
  type Storage AnimationName = Map AnimationName



newtype Active = Active { unActive :: Bool }

instance Component Active where
  type Storage Active = Map Active



data Action = Stand | Walk | Jump
  deriving Show

instance Component Action where
  type Storage Action = Map Action



data Facing = FaceLeft | FaceRight

instance Component Facing where
  type Storage Facing = Map Facing



newtype FastFalling = FastFalling Bool

instance Component FastFalling where
  type Storage FastFalling = Unique FastFalling



newtype Wait = Wait CFloat

instance Component Wait where
  type Storage Wait = Map Wait



data Player = Player

instance Component Player where
  type Storage Player = Unique Player



data Dinosaur = Dinosaur

instance Component Dinosaur where
  type Storage Dinosaur = Map Dinosaur



type PhysicsComponents
  = ( 
      ( Position
      , Velocity
      , CollisionFlags
      , OldPosition
      , OldVelocity
      , OldCollisionFlags
      , OnOneWayPlatform
      )
    , ( AABB
      , JumpSpeed
      , JumpStrafe
      , WalkSpeed
      , WalkAccel
      , TerminalVelocity
      )
    )

initPhysics :: PhysicsComponents
initPhysics = 
    ( 
      ( Position 0
      , Velocity 0
      , CollisionFlags 0
      , Old $ Position 0
      , Old $ Velocity 0
      , Old $ CollisionFlags 0
      , OnOneWayPlatform False
      )
    , ( AABB 0 0 0 0
      , JumpSpeed 0
      , JumpStrafe 0
      , WalkSpeed 0
      , WalkAccel 0
      , TerminalVelocity 0
      )
    )