{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module ECS.Base (
  System'
, World
, initWorld
, module ECS.Components
) where

import ECS.Components

import Apecs

makeWorld "World" [ ''Running
                  , ''Renderer
                  , ''Window
                  , ''Camera
                  , ''Time
                  , ''DT
                  , ''TMap
                  , ''ControlInput
                  , ''PrevControlInput
                  , ''DefaultTexture
                  , ''Textures

                  , ''Sprite
                  , ''Active
                  , ''Action
                  , ''Facing

                  , ''Player
                  , ''FastFalling

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