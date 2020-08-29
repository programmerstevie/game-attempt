{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module ECS.TileMapComponent where


import ECS.PhysicsComponents
import ECS.OrphanInstances()

import Apecs.Core (Component, Storage)
import Apecs.Stores (Global)
import Linear
import Data.Aeson
import Foreign.C.Types (CInt(..))
import Data.Traversable (for)
import qualified Data.HashMap.Strict as HM
import qualified ECS.Utils
import qualified SDL
import Data.Text (Text, unpack)



import qualified Data.Array as Array

type MapTiles = Array.Array (V2 CInt) CInt

data TMap = TMap {
  name_M            :: String
, backgroundPath_M  :: FilePath
, tileTexturePath_M :: FilePath
, tileTextureMap_M  :: HM.HashMap CInt (Maybe (SDL.Rectangle CInt))
, map_M             :: MapTiles
, destRect_M        :: Maybe (SDL.Rectangle CInt)
, playerStartPos_M  :: Position
, exitPos_M         :: Position
, ents_M            :: [(String, [Position])]
}

instance Semigroup TMap where
  (<>) = mappend
instance Monoid TMap where
  mempty = TMap { name_M            = ""
                , backgroundPath_M  = ""
                , tileTexturePath_M = ""
                , tileTextureMap_M  = HM.empty
                , map_M             = Array.array (1, 0) []
                , destRect_M        = Nothing
                , playerStartPos_M  = Position 0 
                , exitPos_M         = Position 0
                , ents_M            = []
                }
instance Component TMap where
  type Storage TMap = Global TMap
instance FromJSON TMap where
  parseJSON = withObject "TileMap" $ \v -> do
    name       :: String   <- v .: "name"
    bckgr_path :: FilePath <- v .: "bckgr_path"
    tile_path  :: FilePath <- v .: "tile_path"

    tile_types :: [Text]   <- v .: "types"
    tiles      :: Object   <- v .: "tiles"
    rect_map <- fmap HM.fromList <$> for tile_types $ \tileName -> do
      tile      :: Object <- tiles .: tileName

      tile_id   :: CInt   <- tile .: "id"
      src_rect  :: Maybe (SDL.Rectangle CInt) 
        <- ECS.Utils.fromCorners <$> tile .: "rect"
      pure (tile_id, src_rect)
    map_size      :: V2 CInt  <- v .: "map_size"
    tile_map_list :: [[CInt]] <- v .: "tile_map"
    player_start_position <- Position <$> v .: "player_start_position"
    exit_position         <- Position <$> v .: "exit_position"

    entity_list :: [Text] <- v .: "entity_list"
    entities :: Object <- v .: "entities"
    entities_formatted <- for entity_list $ \entName -> do
      entPos :: [Position] <- map Position <$> entities .: entName
      pure (unpack entName, entPos)

    pure $ TMap {
      name_M = name
    , backgroundPath_M = bckgr_path
    , tileTexturePath_M = tile_path
    , tileTextureMap_M  = rect_map
    , map_M = Array.listArray (0, map_size - V2 1 1) . concat $ tile_map_list
    , destRect_M = Just $ SDL.Rectangle (SDL.P (V2 0 0)) $ V2 32 32
    , playerStartPos_M = player_start_position
    , exitPos_M = exit_position
    , ents_M = entities_formatted
    }


isOneWayPlatform :: MapTiles -> V2 CInt -> Bool
isOneWayPlatform map_m coords
  | Array.inRange (Array.bounds map_m) coords =
      case map_m Array.! coords of
        3 -> True
        _ -> False
  | otherwise = False


isObstacle :: MapTiles -> V2 CInt -> Bool
isObstacle map_m coords
  | Array.inRange (Array.bounds map_m) coords =
      case map_m Array.! coords of
        1 -> True
        2 -> True
        _ -> False
  | otherwise = True