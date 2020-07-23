{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ECS.GlobalComponents where


import qualified Constants


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



newtype Renderer = Renderer { unRenderer :: SDL.Renderer }

instance Semigroup Renderer where
  (<>) = mappend
instance Monoid Renderer where
  mempty = Renderer $ SDL_T.Renderer nullPtr
instance Component Renderer where
  type Storage Renderer = Global Renderer



newtype Window = Window { unWindow :: SDL.Window }

instance Semigroup Window where
  (<>) = mappend
instance Monoid Window where
  mempty = Window $ SDL_T.Window nullPtr
instance Component Window where
  type Storage Window = Global Window



type MapTiles = Array.Array (V2 CInt) CInt

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
                , destRect_M       = Nothing
                , playerStartPos_M = 0 
                , exitPos_M        = 0
                , ents_M           = []
                }
instance Component TMap where
  type Storage TMap = Global TMap



data Camera = Camera {
  gameCoords_C :: V2 CFloat
, gameSize_C   :: V2 CFloat
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



instance Ix CInt where
  range (m,n) = [m..n]
  index b@(m,_n) i | inRange b i = fromIntegral $ i - m
                   | otherwise   = error "CInt index Error"
  inRange (m, n) i =  (m <= i) && (i <= n)