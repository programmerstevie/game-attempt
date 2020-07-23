{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiWayIf            #-}
module Utils where

-- import qualified

import ECS.Base
import qualified Constants as Cons

import Apecs
import Apecs.Components
import Apecs.Core
import Control.Monad
import Control.Monad.Reader
import qualified SDL
import Linear
import Foreign.C.Types (CFloat, CInt)
import Control.Monad.IO.Class

toSeconds :: (Integral i, Floating f) => i -> f
toSeconds = (/ 1000) . fromIntegral


consoleLog :: MonadIO m => String -> m ()
consoleLog = liftIO . putStrLn

consoleShowLog :: (MonadIO m, Show a) => a -> m ()
consoleShowLog = consoleLog . show


makeRect :: SDL.V2 CInt -> V2 CInt -> SDL.Rectangle CInt
makeRect = SDL.Rectangle . SDL.P

setRectPos :: Maybe (SDL.Rectangle CInt)
           -> V2 CInt
           -> Maybe (SDL.Rectangle CInt)
setRectPos (Just (SDL.Rectangle _ size)) pos = 
  Just $ SDL.Rectangle (SDL.P pos) size
setRectPos Nothing _ = Nothing

modRectPos :: Maybe (SDL.Rectangle CInt)
           -> (V2 CInt -> V2 CInt)
           -> Maybe (SDL.Rectangle CInt)
modRectPos (Just (SDL.Rectangle (SDL.P pos) size)) fun =
  Just $ SDL.Rectangle (SDL.P $ fun pos) size
modRectPos Nothing _ = Nothing

setX, setY :: V2 a -> a -> V2 a
setX (V2 _ vy) v = V2 v vy
setY (V2 vx _) = V2 vx

getX, getY :: V2 a -> a
getX (V2 vx _) = vx
getY (V2 _ vy) = vy

modX, modY :: (a -> a) -> V2 a -> V2 a
modX f (V2 vx vy) = V2 (f vx) vy
modY f (V2 vx vy) = V2 vx (f vy)


modify', ($~~) :: forall w m cx cy. (Get w m cx, Set w m cy)
               => Entity
               -> (cx -> cy)
               -> SystemT w m ()
modify' (Entity ety) f = do
  sx :: Storage cx <- getStore
  sy :: Storage cy <- getStore
  lift $ do
    possible <- explExists sx ety
    when possible $ do
      x <- explGet sx ety
      explSet sy ety (f x)
($~~) = modify'
infixr 2 $~~


modifyM, ($>=) :: forall w m cx cy. (Get w m cx, Set w m cy)
               => Entity
               -> (cx -> SystemT w m cy)
               -> SystemT w m ()
modifyM (Entity ety) f = do
  sx :: Storage cx <- getStore
  sy :: Storage cy <- getStore
  possible <- lift $ explExists sx ety
  when possible $ do
    x <- lift $ explGet sx ety
    y <- f x
    lift $ explSet sy ety y
($>=) = modifyM
infixr 2 $>=


modifyM_, ($>>) :: forall w m c. (Get w m c)
                => Entity
                -> (c -> SystemT w m ())
                -> SystemT w m ()
modifyM_ (Entity ety) f = do
  s :: Storage c <- getStore
  possible <- lift $ explExists s ety
  when possible $ do
    x <- lift $ explGet s ety
    f x
($>>) = modifyM_
infixr 2 $>>


swapV :: V2 a -> V2 a
swapV (V2 vx vy) = V2 vy vx

worldToSdlCoords :: V2 CFloat -> V2 CInt
worldToSdlCoords  = fmap (floor . (*32)) . modY (20 -)

sdlToWorldCoords :: V2 CInt -> V2 CFloat
sdlToWorldCoords = modY (20 -) . fmap ((*32) . fromIntegral)


scaleToWorld :: V2 CInt -> V2 CFloat
scaleToWorld = fmap $ (/ 32) . fromIntegral

scaleToSdl :: V2 CFloat -> V2 CInt
scaleToSdl = fmap $ floor . (* 32)


worldToMapCoords :: V2 CFloat -> V2 CInt
worldToMapCoords = swapV . modY (19 -) . fmap floor

worldYToMapY :: CFloat -> CInt
worldYToMapY = (19 -) . floor
worldXToMapX :: CFloat -> CInt
worldXToMapX = floor

mapYToWorldY :: CInt -> CFloat
mapYToWorldY = fromIntegral . (19 -)
mapXToWorldX :: CInt -> CFloat
mapXToWorldX = fromIntegral


mapToWorldCoords :: V2 CInt -> V2 CFloat
mapToWorldCoords = fmap fromIntegral . modY (19 -) . swapV

elemTimes :: Num n => V2 n -> V2 n -> V2 n
elemTimes (V2 ax ay) (V2 bx by) = V2 (ax * bx) (ay * by)
(>*<) = elemTimes

drawAABB :: AABB -> System' ()
drawAABB AABB{ center   = ctr
             , halfSize = hs 
             , scale    = scl } = do
  let rect = makeRect (worldToSdlCoords $ ctr 
                      + modX negate (scl >*< hs)) $
                        fmap floor $ (scl >*< hs) ^* (2 * Cons.coordsScale)
  Renderer renderer <- get global
  SDL.rendererDrawColor renderer SDL.$= V4 200 0 0 255
  SDL.drawRect renderer (Just rect)

setPos :: Entity -> Position -> System' ()
setPos ety (Position pos) = do
  ety $= Position pos
  hasAABB <- exists ety (Proxy :: Proxy AABB)
  when hasAABB $ do
    aabb :: AABB <- get ety
    ety $= aabb{ center = pos + offset aabb }


lerp :: V2 CFloat -> V2 CFloat -> CFloat -> V2 CFloat
lerp a b t
  | t <= 1 && t >= 0 = a + ((b - a) ^* t)
  | otherwise = error "lerp out of range"

roundVec :: V2 CFloat -> V2 CFloat
roundVec = fmap pixRound 

pixRound :: CFloat -> CFloat
pixRound x
  | ending /= 0.5 = fromIntegral (round bigX) * Cons.onePix
  | even wholeX   = fromIntegral       wholeX * Cons.onePix
  | otherwise     = fromIntegral (wholeX + 1) * Cons.onePix
    where bigX = x / Cons.onePix
          wholeX = floor bigX
          ending = bigX - fromIntegral wholeX