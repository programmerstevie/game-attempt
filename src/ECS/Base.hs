{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module ECS.Base (
  System'
, World
, initWorld
, EntityComponents
, module ECS.Components
) where

import ECS.Components
import ECS.OrphanInstances()

import Apecs

makeWorld "World" [ ''Running
                  , ''CRenderer
                  , ''CWindow
                  , ''Camera
                  , ''Time
                  , ''DT
                  , ''TMap
                  , ''ControlInput
                  , ''PrevControlInput
                  , ''DefaultTexture
                  , ''Textures
                  , ''AnimationMap

                  , ''AnimationName
                  , ''Sprite
                  , ''Animation
                  , ''Active
                  , ''Wait
                  , ''Action
                  , ''Facing
                  , ''FastFalling

                  , ''Player
                  , ''Dinosaur


                  , ''AABB

                  , ''Position
                  , ''Velocity
                  , ''CollisionFlags

                  , ''OldPosition
                  , ''OldVelocity
                  , ''OldCollisionFlags

                  , ''OnOneWayPlatform

                  , ''JumpSpeed
                  , ''JumpStrafe
                  , ''WalkSpeed
                  , ''WalkAccel
                  , ''TerminalVelocity
                  ]

type System' a = System World a


-- Aliases

type EntityComponents = ( Player
                        , Dinosaur
                        , PhysicsComponents
                        , ( FastFalling
                          , Action
                          , Facing
                          , Sprite
                          , Animation
                          , Active
                          )
                        )