{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module DinoBehavior where


import ECS.Base
import qualified EntityUtils
import qualified Utils
import Utils (($>>))
import qualified Controls
import qualified KeyboardController as KeyControl


import Apecs
import Data.Bits
import Control.Applicative (liftA2)
import Control.Monad (when)


updateDinos :: System' ()
updateDinos = 
  cmapM_ $ \(Dinosaur
            , _ :: PhysicsComponents
            , ety :: Entity
            , CollisionFlags flags
            , Not :: Not Wait) -> do
    cantGoRight <- liftA2 (||)
      (EntityUtils.atRightEdge ety)
      (pure $ testBit flags 9)
    cantGoLeft  <- liftA2 (||)
      (EntityUtils.atLeftEdge  ety)
      (pure $ testBit flags 8)
    
    ety $>> \(facing :: Facing) -> do
      turnAround <- case facing of
        FaceRight -> do
          when cantGoRight $ ety $= FaceLeft
          pure cantGoRight
        FaceLeft  -> do
          when cantGoLeft  $ ety $= FaceRight
          pure cantGoLeft
      when turnAround $ do
        ety $= Stand
        ety $~ \(Velocity v) -> Velocity $ Utils.setX v 0
        EntityUtils.freeze ety 1
      KeyControl.keyboardControl ety $
        if turnAround
        then Controls.noControls
        else case facing of
          FaceRight -> Controls.rightControl
          FaceLeft  -> Controls.leftControl