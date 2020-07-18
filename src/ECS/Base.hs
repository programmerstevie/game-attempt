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

import qualified SDL

import Apecs

makeWorld "World" [ ''Running
                  , ''Time
                  , ''Renderer
                  , ''Window
                  , ''TMap
                  , ''ControlInput
                  , ''PrevControlInput
                  
                  , ''KeyboardControl
                  , ''Sprite
                  , ''Active
                  , ''Action
                  , ''Facing

                  , ''Player
                  , ''Dinosaur

                  , ''Gravity
                  , ''DT


                  , ''AABB

                  , ''Position
                  , ''Velocity
                  , ''PushesRightWall
                  , ''PushesLeftWall
                  , ''OnGround
                  , ''AtCeiling

                  , ''OldPosition
                  , ''OldVelocity
                  , ''PushedRightWall
                  , ''PushedLeftWall
                  , ''WasOnGround
                  , ''WasAtCeiling

                  , ''JumpHeight
                  , ''JumpSpeed
                  , ''JumpAccel
                  , ''WalkSpeed
                  , ''WalkAccel
                  , ''TerminalVelocity
                  ]

type System' a = System World a