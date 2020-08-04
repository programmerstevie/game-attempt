{-# LANGUAGE TypeFamilies #-}
module ECS.PhysicsComponents where



import Apecs.Core (Component, Storage)
import Apecs.Stores (Map)
import Linear
import Foreign.C.Types (CShort, CFloat)



data AABB = AABB { center_aabb   :: V2 CFloat
                 , halfSize_aabb :: V2 CFloat
                 , offset_aabb   :: V2 CFloat
                 , scale_aabb    :: V2 CFloat }

instance Component AABB where
  type Storage AABB = Map AABB



newtype Position = Position { unPosition :: V2 CFloat }

instance Component Position where
  type Storage Position = Map Position



newtype Velocity = Velocity { unVelocity :: V2 CFloat }

instance Component Velocity where
  type Storage Velocity = Map Velocity



newtype CollisionFlags = CollisionFlags { unCollisionFlags :: CShort }

instance Component CollisionFlags where
  type Storage CollisionFlags = Map CollisionFlags

{- 
0  PushesLeft
1  PushesRight
2  PushesBottom
3  PushesTop

4  PushesLeftObject
5  PushesRightObject
6  PushesBottomObject
7  PushesTopObject

8  PushesLeftTile
9  PushesRightTile
10 PushesBottomTile
11 PushesTopTile
-}



newtype OnOneWayPlatform = OnOneWayPlatform { unOnOneWayPlatform :: Bool }

instance Component OnOneWayPlatform where
  type Storage OnOneWayPlatform = Map OnOneWayPlatform



newtype Old a = Old { unOld :: a }

instance (Component a) => Component (Old a) where
  type Storage (Old a) = Map (Old a)

type OldPosition       = Old Position
type OldVelocity       = Old Velocity
type OldCollisionFlags = Old CollisionFlags



newtype JumpHeight = JumpHeight { unJumpHeight :: CFloat }

instance Component JumpHeight where
  type Storage JumpHeight = Map JumpHeight



newtype JumpSpeed = JumpSpeed { unJumpSpeed :: CFloat }

instance Component JumpSpeed where
  type Storage JumpSpeed = Map JumpSpeed



newtype WalkSpeed = WalkSpeed { unWalkSpeed :: CFloat }

instance Component WalkSpeed where
  type Storage WalkSpeed = Map WalkSpeed



newtype WalkAccel = WalkAccel { unWalkAccel :: CFloat }

instance Component WalkAccel where
  type Storage WalkAccel = Map WalkAccel



newtype JumpStrafe = JumpStrafe { unJumpStrafe :: CFloat }

instance Component JumpStrafe where
  type Storage JumpStrafe = Map JumpStrafe



newtype TerminalVelocity = TerminalVelocity { unTerminalVelocity :: CFloat }

instance Component TerminalVelocity where
  type Storage TerminalVelocity = Map TerminalVelocity
