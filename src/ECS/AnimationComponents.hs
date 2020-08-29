{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
module ECS.AnimationComponents where


import qualified ECS.Utils


import Apecs.Core (Component, Storage)
import Apecs.Stores (Map, Global)
import Data.Traversable (for)
import Data.Text (Text, unpack)
import Data.Aeson
import Foreign.C.Types (CFloat(..), CInt(..))
import Linear
import qualified Data.HashMap.Strict as HM
import qualified SDL



data Sprite = Sprite { filePath_S :: FilePath
                     , srcRect_S  :: Maybe (SDL.Rectangle CInt)
                     , destRect_S :: Maybe (SDL.Rectangle CInt)
                     , flip_S     :: V2 Bool
                     }

instance Component Sprite where
  type Storage Sprite = Map Sprite



data Animation = Animation { name_A :: String
                           , filePath_A  :: FilePath
                           , srcRects_A  :: [Maybe (SDL.Rectangle CInt)]
                           , destRects_A :: [Maybe (SDL.Rectangle CInt)]
                           , length_A :: Int
                           , delta_A :: CFloat
                           , time0_A :: CFloat
                           }

instance Component Animation where
  type Storage Animation = Map Animation



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
    def        :: String <- v .: "default"
    animations :: Object <- v .: "animations"
    animMap <- fmap HM.fromList . for types $ \t -> do
      animation  :: Object   <- animations .: t
      dT         :: CFloat   <- CFloat <$> animation .: "dT"
      file_path  :: FilePath <- animation .: "file_path"

      src_rects  :: [Maybe (SDL.Rectangle CInt)]
        <- map ECS.Utils.fromCorners <$> animation .: "src_coords"
      let dest_rects = map
              (ECS.Utils.modRectSize . fmap $ round . (* scale) . fromIntegral)
               src_rects
          name' = name ++ '.' : unpack t
      pure ( name',
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
    pure . AnimationMap $ 
      case HM.lookup (name ++ '.' : def) animMap of
        Just anim -> HM.insert (name ++ ".default") anim animMap
        Nothing   -> error "NO DEFAULT ANIMATION"