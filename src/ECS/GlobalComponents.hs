{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ECS.GlobalComponents where


import ECS.AnimationComponents


import Apecs.Core (Component, Storage)
import Apecs.Stores (Global)
import Control.Monad (forM)
import Data.Aeson
import Data.Ix
import Data.Text (Text, unpack)
import Foreign.Ptr (nullPtr)
import Foreign.C.Types (CFloat(..), CInt(..))
import Linear
import qualified Data.Array as Array
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



newtype Textures = Textures { unTextures :: HM.HashMap String SDL.Texture }

instance Semigroup Textures where
  (<>) = mappend
instance Monoid Textures where
  mempty = Textures HM.empty
instance Component Textures where
  type Storage Textures = Global Textures



newtype AnimationMap = AnimationMap { 
  unAnimationMap :: HM.HashMap String Animation 
}

instance Semigroup AnimationMap where
  (<>) = mappend
instance Monoid AnimationMap where
  mempty = AnimationMap HM.empty
instance Component AnimationMap where
  type Storage AnimationMap = Global AnimationMap

instance FromJSON AnimationMap where
  parseJSON = withObject "Animation" $ \v -> do
    name       :: String <- v .: "name"
    scale      :: CFloat <- CFloat <$> v .: "scale"
    types      :: [Text] <- v .: "types"
    def        :: String <- v.: "default"
    animations :: Object <- v .: "animations"
    animMap <- fmap HM.fromList . forM types $ \t -> do
      animation  :: Object <- animations .: t
      dT         :: CFloat <- CFloat <$> animation .: "dT"
      file_path  :: FilePath <- animation .: "file_path"

      src_rects  :: [Maybe (SDL.Rectangle CInt)]
        <- map fromCorners <$> animation .: "src_coords"
      let dest_rects = map
               (modRectSize . fmap $ floor . (* scale) . fromIntegral)
                src_rects
          name' = name ++ '.' : unpack t
      return ( name',
               Animation {
                 name_A      = name'
               , filePath_A  = file_path
               , srcRects_A  = src_rects
               , destRects_A = dest_rects
               , length_A    = length src_rects
               , delta_A     = dT
               , time0_A     = 0
               }
             )
    return . AnimationMap $ 
      case HM.lookup (name ++ '.' : def) animMap of
        Just anim -> HM.insert (name ++ '.' : "default") anim animMap
        Nothing   -> error "NO DEFAULT ANIMATION"

    where
      fromCorners :: ((Int, Int), (Int, Int)) -> Maybe (SDL.Rectangle CInt)
      fromCorners ((x0, y0), (x1, y1)) = 
        let x = fromIntegral x0
            y = fromIntegral y0
            w = fromIntegral $ x1 - x0
            h = fromIntegral $ y1 - y0
        in Just . SDL.Rectangle (SDL.P $ V2 x y) $ V2 w h

      modRectSize :: (V2 CInt -> V2 CInt)
                -> Maybe (SDL.Rectangle CInt)
                -> Maybe (SDL.Rectangle CInt)
      modRectSize fun (Just (SDL.Rectangle p size)) = 
        Just $ SDL.Rectangle p $ fun size
      modRectSize _ Nothing = Nothing



data DefaultTexture = DefaultTexture SDL.Texture | DefaultTextureError

instance Semigroup DefaultTexture where
  (<>) = mappend
instance Monoid DefaultTexture where
  mempty = DefaultTextureError
instance Component DefaultTexture where
  type Storage DefaultTexture = Global DefaultTexture


newtype ICInt = ICInt { unICInt :: CInt }
  deriving (Real, Enum, Ord, Eq, Integral, Num)

instance Ix ICInt where
  range (m,n) = [m..n]
  index b@(m,_n) i | inRange b i = fromIntegral $ i - m
                   | otherwise   = error "CInt index Error"
  inRange (m, n) i =  (m <= i) && (i <= n)