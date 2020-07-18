{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ECS.GlobalComponents where

import qualified Constants

import Apecs.Core (Component, Storage)
import Apecs.Stores (Global)
import qualified Data.Array as Array
import Foreign.Ptr (nullPtr)
import Foreign.C.Types (CInt, CFloat)
import qualified SDL
import qualified SDL.Internal.Types as SDL_T
import Data.Ix
import Linear



newtype Running = Running Bool deriving Show

instance Semigroup Running where
  (<>) = mappend
instance Monoid Running where
  mempty = Running False
instance Component Running where
  type Storage Running = Global Running



newtype Time = Time CFloat deriving Show

instance Semigroup Time where
  (<>) = mappend
instance Monoid Time where
  mempty = Time 0
instance Component Time where
  type Storage Time = Global Time


newtype DT = DT CFloat deriving Show

instance Semigroup DT where
  (<>) = mappend
instance Monoid DT where
  mempty = DT 0
instance Component DT where
  type Storage DT = Global DT


newtype Renderer = Renderer SDL.Renderer

instance Semigroup Renderer where
  (<>) = mappend
instance Monoid Renderer where
  mempty = Renderer $ SDL_T.Renderer nullPtr
instance Component Renderer where
  type Storage Renderer = Global Renderer



newtype Window = Window SDL.Window

instance Semigroup Window where
  (<>) = mappend
instance Monoid Window where
  mempty = Window $ SDL_T.Window nullPtr
instance Component Window where
  type Storage Window = Global Window


newtype Gravity = Gravity (V2 CFloat)

instance Semigroup Gravity where
  (<>) = mappend
instance Monoid Gravity where
  mempty = Gravity $ V2 0 (-Constants.gravity)
instance Component Gravity where
  type Storage Gravity = Global Gravity


type MapTiles = Array.Array (V2 CInt) CInt

data TMap = TMap {
  dirtTex_M  :: SDL.Texture
, grassTex_M :: SDL.Texture
, srcRect_M  :: Maybe (SDL.Rectangle CInt)
, destRect_M :: Maybe (SDL.Rectangle CInt)
, map_M      :: Maybe MapTiles -- 20 25
} | TMapNULL

instance Semigroup TMap where
  (<>) = mappend
instance Monoid TMap where
  mempty = TMapNULL
instance Component TMap where
  type Storage TMap = Global TMap


data ControlInput = ControlInput {
  upKey    :: Bool
, leftKey  :: Bool
, downKey  :: Bool
, rightKey :: Bool
}

instance Semigroup ControlInput where
  (<>) = mappend
instance Monoid ControlInput where
  mempty = ControlInput False False False False
instance Component ControlInput where
  type Storage ControlInput = Global ControlInput


newtype PrevControlInput = PrevControlInput ControlInput
  deriving (Semigroup, Monoid)

instance Component PrevControlInput where
  type Storage PrevControlInput = Global PrevControlInput



instance Ix CInt where
  range (m,n) = [m..n]
  index b@(m,_n) i | inRange b i = fromIntegral $ i - m
                   | otherwise   = error "CInt index Error"
  inRange (m, n) i =  (m <= i) && (i <= n)