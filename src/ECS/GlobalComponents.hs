{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ECS.GlobalComponents where


import Apecs.Core (Component, Storage)
import Apecs.Stores (Global)
import Data.HashMap.Strict as HM
import Data.Ix
import Foreign.Ptr (nullPtr)
import Foreign.C.Types (CInt, CFloat)
import Linear
import qualified Data.Array as Array
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



type MapTiles = Array.Array (V2 ICInt) CInt

data TMap = TMap {
  map_M            :: MapTiles
, srcRect_M        :: Maybe (SDL.Rectangle CInt)
, destRect_M       :: Maybe (SDL.Rectangle CInt)
, playerStartPos_M :: V2 CFloat
, exitPos_M        :: V2 CFloat
, ents_M           :: [(String, V2 CFloat)]
}

instance Semigroup TMap where
  (<>) = mappend
instance Monoid TMap where
  mempty = TMap { map_M            = Array.array (1, 0) []
                , srcRect_M        = Nothing
                , destRect_M       = Nothing
                , playerStartPos_M = 0 
                , exitPos_M        = 0
                , ents_M           = []
                }
instance Component TMap where
  type Storage TMap = Global TMap



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



newtype Textures = Textures (HM.HashMap String SDL.Texture)

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


newtype ICInt = ICInt CInt
  deriving (Real, Enum, Ord, Eq, Integral, Num)

instance Ix ICInt where
  range (m,n) = [m..n]
  index b@(m,_n) i | inRange b i = fromIntegral $ i - m
                   | otherwise   = error "CInt index Error"
  inRange (m, n) i =  (m <= i) && (i <= n)