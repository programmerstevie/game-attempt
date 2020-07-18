module Action where

import ECS.Base
import qualified Utils
import Utils (($>>))

import Apecs
import Foreign.C.Types (CFloat)
import Control.Monad (when)


correctXVel :: Entity -> CFloat -> CFloat -> System' ()
correctXVel ety speed friction = ety $>> \(Velocity vel) -> 
  when (abs (Utils.getX vel) > speed) $ do
    DT dT <- get global
    let sign = signum $ Utils.getX vel
    set ety . Velocity $ Utils.modX (+ friction * dT * sign) vel
    Velocity vel' <- get ety
    let sign0 = signum $ Utils.getX vel - (speed * sign)
        sign1 = signum $ Utils.getX vel' - (speed * sign)
    when (sign0 /= sign1) $
      ety $= Velocity (Utils.setX vel' (speed * sign))


correctYVel :: Entity -> CFloat -> CFloat -> System' ()
correctYVel ety speed friction = ety $>> \(Velocity vel) -> 
  when (abs (Utils.getY vel) > speed) $ do
    DT dT <- get global
    let sign = signum $ Utils.getY vel
    set ety . Velocity $ Utils.modY (+ friction * dT * sign) vel
    Velocity vel' <- get ety
    let sign0 = signum $ Utils.getY vel - (speed * sign)
        sign1 = signum $ Utils.getY vel' - (speed * sign)
    when (sign0 /= sign1) $
      ety $= Velocity (Utils.setY vel' (speed * sign))


jumpAction :: Entity -> System' ()
jumpAction ety = do
  JumpSpeed jmpspd <- get ety
  ety $= Jump
  ety $~ \(Velocity vel) -> Velocity $ Utils.setY vel jmpspd