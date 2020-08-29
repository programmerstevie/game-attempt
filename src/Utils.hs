{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiWayIf            #-}
module Utils where

-- import qualified

import ECS.Base
import qualified Constants as Cons


import Apecs
import Apecs.Core
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import qualified SDL
import qualified Data.Array as Array
import Linear
import Foreign.C.Types (CFloat, CInt)


toSeconds :: (Integral i, Floating f) => i -> f
toSeconds = (/ 1000) . fromIntegral


consoleLog :: MonadIO m => String -> m ()
consoleLog = liftIO . putStrLn

consoleShowLog :: (MonadIO m, Show a) => a -> m ()
consoleShowLog = consoleLog . show


getRectSize :: Maybe (SDL.Rectangle CInt) -> System' (V2 CInt)
getRectSize Nothing = SDL.get . SDL.windowSize . unWindow =<< get global
getRectSize (Just (SDL.Rectangle _ size)) = pure size


makeRect :: V2 CInt -> V2 CInt -> SDL.Rectangle CInt
makeRect = SDL.Rectangle . SDL.P

setRectPos :: Maybe (SDL.Rectangle CInt)
           -> V2 CInt
           -> Maybe (SDL.Rectangle CInt)
setRectPos (Just (SDL.Rectangle _ size)) pos = 
  Just $ SDL.Rectangle (SDL.P pos) size
setRectPos Nothing _ = Nothing

modRectPos :: (V2 CInt -> V2 CInt)
           -> Maybe (SDL.Rectangle CInt)
           -> Maybe (SDL.Rectangle CInt)
modRectPos fun (Just (SDL.Rectangle (SDL.P pos) size)) =
  Just $ SDL.Rectangle (SDL.P $ fun pos) size
modRectPos _ Nothing = Nothing

modRectSize :: (V2 CInt -> V2 CInt)
           -> Maybe (SDL.Rectangle CInt)
           -> Maybe (SDL.Rectangle CInt)
modRectSize fun (Just (SDL.Rectangle (SDL.P pos) size)) =
  Just $ SDL.Rectangle (SDL.P pos) $ fun size
modRectSize _ Nothing = Nothing

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


scaleToWorld :: V2 CInt -> V2 CFloat
scaleToWorld = fmap $ (/ Cons.coordsScale) . fromIntegral

scaleToSdl :: V2 CFloat -> V2 CInt
scaleToSdl = fmap $ round . (* Cons.coordsScale)


worldToMapCoords :: MapTiles -> V2 CFloat -> V2 CInt
worldToMapCoords map_m = swapV . modY (yHeight -) . fmap floor
  where yHeight = getX . snd $ Array.bounds map_m


mapToWorldCoords :: MapTiles -> V2 CInt -> V2 CFloat
mapToWorldCoords map_m = fmap fromIntegral . modY (yHeight -) . swapV
  where yHeight = getX . snd $ Array.bounds map_m


worldToMapY, worldToMapX ::  MapTiles -> CFloat -> CInt
worldToMapY map_m y = getX $ worldToMapCoords map_m (V2 0 y)
worldToMapX map_m x = getY $ worldToMapCoords map_m (V2 x 0)


mapToWorldY, mapToWorldX :: MapTiles -> CInt -> CFloat
mapToWorldY map_m y = getY $ mapToWorldCoords map_m (V2 y 0)
mapToWorldX map_m x = getX $ mapToWorldCoords map_m (V2 0 x)


elemTimes, (^^*) :: Num n => V2 n -> V2 n -> V2 n
elemTimes = liftA2 (*)
(^^*) = elemTimes


elemDiv, (^^/) :: V2 CFloat -> V2 CFloat -> V2 CFloat
elemDiv = liftA2 (/)
(^^/) = elemDiv


setPos :: Entity -> Position -> System' ()
setPos ety (Position pos) = do
  ety $= Position pos
  hasAABB <- exists ety (Proxy :: Proxy AABB)
  when hasAABB $ do
    aabb :: AABB <- get ety
    ety $= aabb{ center_aabb = pos + offset_aabb aabb }


lerp :: V2 CFloat -> V2 CFloat -> CFloat -> V2 CFloat
lerp a b t
  | t <= 1 && t >= 0 = a + ((b - a) ^* t)
  | otherwise = error "lerp out of range"

roundVec :: V2 CFloat -> V2 CFloat
roundVec = fmap pixRound 

pixRound :: CFloat -> CFloat
pixRound x
  | ending /= 0.5 = fromIntegral (round bigX :: CInt) * Cons.onePix
  | even wholeX   = fromIntegral       wholeX * Cons.onePix
  | otherwise     = fromIntegral (wholeX + 1) * Cons.onePix
    where bigX = x / Cons.onePix
          wholeX = floor bigX :: CInt
          ending = bigX - fromIntegral wholeX
