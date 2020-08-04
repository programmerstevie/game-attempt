{-# LANGUAGE TypeFamilies #-}
module ECS.AnimationComponents where


import Apecs.Core (Component, Storage)
import Apecs.Stores (Map)
import qualified SDL
import Foreign.C.Types (CFloat, CInt)
import Linear



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
