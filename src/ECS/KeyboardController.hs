{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{- HLINT ignore "Redundant do" -}
module ECS.KeyboardController where

-- import qualified

import ECS.Base
import qualified Utils
import Utils (($~~), ($>>))
import qualified Action
import qualified Constants

import Apecs
import Control.Monad
import qualified SDL
import Linear


data KeyInput = UpKey | LeftKey | DownKey | RightKey
  deriving Enum

updatePrevControls :: System' ()
updatePrevControls = set global =<< (PrevControlInput <$> get global)

setControls :: SDL.Scancode -> SDL.InputMotion -> System' ()
setControls keyScanCode motion = do
  let pressed = case motion of
          SDL.Pressed  -> True
          SDL.Released -> False
  global $~ \(controls :: ControlInput) ->
    case keyScanCode of
        SDL.ScancodeSpace -> controls { upKey = pressed }
        SDL.ScancodeW -> controls { upKey    = pressed }
        SDL.ScancodeA -> controls { leftKey  = pressed }
        SDL.ScancodeS -> controls { downKey  = pressed }
        SDL.ScancodeD -> controls { rightKey = pressed }
        _ -> controls

pressed :: (ControlInput -> Bool) -> System' Bool
pressed key = do
  (controls :: ControlInput, PrevControlInput prevControls) <- get global
  pure $ key controls && not (key prevControls)


released :: (ControlInput -> Bool) -> System' Bool
released key = do
  (controls :: ControlInput, PrevControlInput prevControls) <- get global
  pure $ not (key controls) && key prevControls


state :: (ControlInput -> Bool) -> System' Bool
state key = do
  controls :: ControlInput <- get global
  pure $ key controls


-- HANDLE KEYBOARD STUFF -- 

keyboardHandle :: System' ()
keyboardHandle = do
  controls :: ControlInput <- get global
  cmapM_ $ \( KeyboardControl
            , _ :: PhysicsComponents
            , action :: Action
            , ety :: Entity) ->
    handleAction action ety controls
 where
  handleAction = \case
    Stand -> handleStand 
    Walk  -> handleWalk
    Jump  -> handleJump


handleStand :: Entity -> ControlInput -> System' ()
handleStand ety = movement
  where
    movement controls
      | upKey controls =
          Action.jumpAction ety
      | leftKey controls /= rightKey controls =
          ety $= Walk
      | downKey controls = ety $>>
          \(OnOneWayPlatform onOneWayPlatform, Position pos) ->
          when onOneWayPlatform $ do
            --Utils.consoleLog "whoop!"
            Utils.setPos ety $ Position $
              Utils.modY (subtract Constants.oneWayPlatformThreshold) pos
            ety $= OnGround False
      | otherwise = pure ()

handleWalk :: Entity -> ControlInput -> System' ()
handleWalk ety = movement
  where
    movement controls
      | upKey controls =
          Action.jumpAction ety
      | rightKey controls == leftKey controls =
          ety $= Stand
      | downKey controls = ety $>>
          \(OnOneWayPlatform onOneWayPlatform, Position pos) ->
          when onOneWayPlatform $ do
            --Utils.consoleLog "whoosh!"
            Utils.setPos ety $ Position $
              Utils.modY (subtract Constants.oneWayPlatformThreshold) pos
            ety $= Jump
      | otherwise = do
          DT dT <- get global
          ety $>> \(Velocity vel, WalkAccel wlkacc) -> do
            let sign = if rightKey controls then 1 else -1
            ety $= Velocity (vel + sign *^ V2 (wlkacc * dT) 0)


handleJump :: Entity -> ControlInput -> System' ()
handleJump ety = movement
  where
    movement controls = do
      DT dT <- get global
      when (leftKey controls /= rightKey controls) $ do
        ety $>> \(Velocity vel, JumpAccel jmpacc) -> do
          let sign = if rightKey controls then 1 else -1
          ety $= Velocity (Utils.modX (+ sign * jmpacc * dT) vel)
      ety $>> \(Velocity vel) ->
        unless (upKey controls || Utils.getY vel <= 0) $
          Action.correctYVel ety Constants.minJumpSpeed (-800)
      when (downKey controls) $ do
        Gravity g <- get global
        ety $~ \(Velocity vel) -> Velocity (vel + g ^* (dT * 2))

