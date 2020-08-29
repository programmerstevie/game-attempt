{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
module KeyboardController where

-- import qualified

import ECS.Base
import qualified Utils
import Utils (($>>))
import qualified Action
import qualified EntityUtils
import qualified Constants as Cons

import Apecs
import Control.Monad
import Data.Bits
import qualified SDL
import Linear


updatePrevControls :: System' ()
updatePrevControls = set global =<< (PrevControlInput <$> get global)


setControls :: SDL.Scancode -> SDL.InputMotion -> System' ()
setControls keyScanCode motion = do
  let pressed_ = case motion of
          SDL.Pressed  -> True
          SDL.Released -> False
  global $~ \(controls :: ControlInput) ->
    case keyScanCode of
        SDL.ScancodeSpace -> controls { upKey = pressed_ }
        SDL.ScancodeW     -> controls { upKey = pressed_ }
        SDL.ScancodeA -> controls { leftKey  = pressed_ }
        SDL.ScancodeS -> controls { downKey  = pressed_ }
        SDL.ScancodeD -> controls { rightKey = pressed_ }
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
keyboardHandle =
  cmapM_ $ \(Player , ety :: Entity) -> get global >>= keyboardControl ety



keyboardControl :: Entity -> ControlInput -> System' ()
keyboardControl ety controls = do
  actionControllable <- exists ety (Proxy :: Proxy Action)
  when actionControllable $ do
    action :: Action <- get ety
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
            Utils.setPos ety $ Position $
              Utils.modY (subtract Cons.oneWayPlatformThreshold) pos
            ety $~ \(CollisionFlags coll) -> CollisionFlags $ clearBit coll 10
      | otherwise = pure ()


handleWalk :: Entity -> ControlInput -> System' ()
handleWalk ety = movement
  where
    movement controls = do
      if
        | upKey controls ->
            Action.jumpAction ety
        | rightKey controls == leftKey controls ->
            ety $= Stand
        | otherwise -> do
            DT dT <- get global
            ety $>> \(Velocity vel, WalkAccel wlkacc) -> do
              let sign = if rightKey controls then 1 else -1
              ety $= Velocity (vel + sign *^ V2 (wlkacc * dT) 0)
      when (downKey controls) $
        ety $>> \(OnOneWayPlatform onOneWayPlatform, Position pos) ->
          when onOneWayPlatform $ do
            Utils.setPos ety $ Position $
              Utils.modY (subtract Cons.oneWayPlatformThreshold) pos
            ety $= Jump


handleJump :: Entity -> ControlInput -> System' ()
handleJump ety = movement
  where
    movement controls = do
      DT dT <- get global
      when (leftKey controls /= rightKey controls) $
        ety $>> \(Velocity vel, JumpStrafe jmpstrf) -> do
          let sign = if rightKey controls then 1 else -1
          ety $= Velocity (vel + V2 (sign * jmpstrf * dT) 0)
      ety $>> \(Velocity vel) ->
        unless (upKey controls || Utils.getY vel <= 0) $
          Action.correctYVel ety Cons.minJumpSpeed (-800)
      when (downKey controls) $ do
        ety $= FastFalling True
        ety $~ \(Velocity vel) ->
          Velocity $ vel + V2 0 (Cons.gravity * (dT * 2))