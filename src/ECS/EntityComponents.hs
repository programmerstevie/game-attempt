{-# LANGUAGE TypeFamilies #-}
module ECS.EntityComponents where

import ECS.PhysicsComponents
import ECS.PlayerComponents

import Apecs.Core (Component, Storage)
import Apecs.Stores (Map)
import qualified SDL
import Linear
import Foreign.C.Types (CInt, CFloat)




newtype Active = Active { unActive :: Bool }

instance Component Active where
  type Storage Active = Map Active



data Action = Stand | Walk | Jump

instance Component Action where
  type Storage Action = Map Action



data Facing = FaceLeft | FaceRight

instance Component Facing where
  type Storage Facing = Map Facing



data Dinosaur = Dinosaur

instance Component Dinosaur where
  type Storage Dinosaur = Map Dinosaur



data Sprite = Sprite { filePath_S :: FilePath
                     , srcRect_S  :: Maybe (SDL.Rectangle CInt)
                     , destRect_S :: Maybe (SDL.Rectangle CInt) 
                     , flip_S     :: V2 Bool
                     }

instance Component Sprite where
  type Storage Sprite = Map Sprite



data Animation = Animation { filePath_A  :: FilePath
                           , srcRects_A  :: [Maybe (SDL.Rectangle CInt)]
                           , destRects_A :: [Maybe (SDL.Rectangle CInt)]
                           , delta_A     :: CFloat
                           , time0_A     :: CFloat
                           , action_A    :: Maybe Action
                           }

instance Component Animation where
  type Storage Animation = Map Animation



-- Aliases

type EntityComponents = ( Player
                        , Dinosaur
                        , ( PhysicsComponents
                          , Action
                          , Facing
                          , Sprite
                          , Animation
                          , Active
                          )
                        )

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