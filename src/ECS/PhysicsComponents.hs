{-# LANGUAGE TypeFamilies #-}
module ECS.PhysicsComponents where



import Apecs.Core (Component, Storage)
import Apecs.Stores (Map, Unique)
import Linear
import Foreign.C.Types (CInt, CFloat)



data AABB = AABB { center   :: V2 CFloat
                 , halfSize :: V2 CFloat
                 , offset   :: V2 CFloat }

instance Component AABB where
  type Storage AABB = Map AABB



newtype Position = Position (V2 CFloat)

instance Component Position where
  type Storage Position = Map Position



newtype Velocity = Velocity (V2 CFloat)

instance Component Velocity where
  type Storage Velocity = Map Velocity



newtype PushesRightWall = PushesRightWall Bool

instance Component PushesRightWall where
  type Storage PushesRightWall = Map PushesRightWall


newtype PushesLeftWall = PushesLeftWall Bool

instance Component PushesLeftWall where
  type Storage PushesLeftWall = Map PushesLeftWall



newtype OnGround = OnGround Bool

instance Component OnGround where
  type Storage OnGround = Map OnGround



newtype AtCeiling = AtCeiling Bool

instance Component AtCeiling where
  type Storage AtCeiling = Map AtCeiling


  
newtype Old a = Old a

instance (Component a) => Component (Old a) where
  type Storage (Old a) = Map (Old a)

type OldPosition     = Old Position
type OldVelocity     = Old Velocity
type PushedRightWall = Old PushesRightWall
type PushedLeftWall  = Old PushesLeftWall
type WasOnGround     = Old OnGround
type WasAtCeiling    = Old AtCeiling



newtype JumpHeight = JumpHeight CFloat

instance Component JumpHeight where
  type Storage JumpHeight = Map JumpHeight



newtype JumpSpeed = JumpSpeed CFloat

instance Component JumpSpeed where
  type Storage JumpSpeed = Map JumpSpeed



newtype WalkSpeed = WalkSpeed CFloat

instance Component WalkSpeed where
  type Storage WalkSpeed = Map WalkSpeed



newtype WalkAccel = WalkAccel CFloat

instance Component WalkAccel where
  type Storage WalkAccel = Map WalkAccel



newtype JumpAccel = JumpAccel CFloat

instance Component JumpAccel where
  type Storage JumpAccel = Map JumpAccel



newtype TerminalVelocity = TerminalVelocity CFloat

instance Component TerminalVelocity where
  type Storage TerminalVelocity = Map TerminalVelocity
