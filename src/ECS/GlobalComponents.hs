{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ECS.GlobalComponents where

import ECS.OrphanInstances()


import Apecs.Core (Component, Storage)
import Apecs.Stores (Global)
import Foreign.Ptr (nullPtr)
import Foreign.C.Types (CFloat(..))
import Linear
import qualified Data.HashMap.Strict as HM
import qualified SDL
import qualified SDL.Internal.Types as SDL_T



newtype Running = Running { unRunning :: Bool } deriving Show

instance Semigroup Running where
  (<>) = mappend
instance Monoid Running where
  mempty = Running False
instance Component Running where
  type Storage Running = Global Running



newtype Time = Time { unTime :: CFloat } deriving Show

instance Semigroup Time where
  (<>) = mappend
instance Monoid Time where
  mempty = Time 0
instance Component Time where
  type Storage Time = Global Time



newtype DT = DT { unDT :: CFloat } deriving Show

instance Semigroup DT where
  (<>) = mappend
instance Monoid DT where
  mempty = DT 0
instance Component DT where
  type Storage DT = Global DT



newtype CRenderer = CRenderer { unRenderer :: SDL.Renderer }

instance Semigroup CRenderer where
  (<>) = mappend
instance Monoid CRenderer where
  mempty = CRenderer $ SDL_T.Renderer nullPtr
instance Component CRenderer where
  type Storage CRenderer = Global CRenderer



newtype CWindow = CWindow { unWindow :: SDL.Window }

instance Semigroup CWindow where
  (<>) = mappend
instance Monoid CWindow where
  mempty = CWindow $ SDL_T.Window nullPtr
instance Component CWindow where
  type Storage CWindow = Global CWindow



data Camera = Camera {
  gameCoords_C :: V2 CFloat
, size_C       :: V2 CFloat
}

instance Semigroup Camera where
  (<>) = mappend
instance Monoid Camera where
  mempty = Camera 0 0
instance Component Camera where
  type Storage Camera = Global Camera



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



newtype Textures = Textures { unTextures :: HM.HashMap String SDL.Texture }

instance Semigroup Textures where
  (<>) = mappend
instance Monoid Textures where
  mempty = Textures HM.empty
instance Component Textures where
  type Storage Textures = Global Textures



data DefaultTexture = DefaultTexture SDL.Texture | DefaultTextureError

instance Semigroup DefaultTexture where
  (<>) = mappend
instance Monoid DefaultTexture where
  mempty = DefaultTextureError
instance Component DefaultTexture where
  type Storage DefaultTexture = Global DefaultTexture
