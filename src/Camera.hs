{-# LANGUAGE ScopedTypeVariables #-}
module Camera where


import ECS.Base
import qualified Constants as Cons
import qualified Utils
import qualified Renderer
import Utils ((^^*), (^^/), ($~~))


import Apecs
import Control.Applicative
import Linear
import qualified SDL
import qualified Data.Array as Array
import Foreign.C.Types (CFloat, CInt)



updateCamera :: System' ()
updateCamera = cmapM_ $ \(Player, aabb :: AABB) -> do
  scaleCoords <- coordsScale
  camCtr <- (^^/ scaleCoords) . fmap fromIntegral <$> Renderer.getViewPortSize
  global $~ \(camera :: Camera) -> 
    camera{ gameCoords_C = center_aabb aabb - camCtr ^/ 2 }
  global $~~ \(camera :: Camera, gameMap :: TMap) ->
    clampCameraToMap (map_M gameMap) camera
  Renderer.setCamViewPort


coordsScale :: System' (V2 CFloat)
coordsScale = do
  Camera{ size_C = sizeC } <- get global
  vpSize <- Renderer.getViewPortSize
  pure $ liftA2 ((/) . fromIntegral) vpSize sizeC


worldToCamera :: Camera -> V2 CFloat -> V2 CFloat
worldToCamera Camera{ gameCoords_C = coords_C } coords = coords - coords_C


worldToSdlCoords :: V2 CFloat -> System' (V2 CInt)
worldToSdlCoords coords = do
  camera :: Camera <- get global
  V2 _ vpY <- fmap fromIntegral <$> Renderer.getViewPortSize
  let camTrans = worldToCamera camera coords
  scaledViewPort <- coordsScale
  pure $ fmap round $ Utils.modY (vpY -) $ camTrans ^^* scaledViewPort


mapToSdlCoords :: MapTiles -> V2 CInt -> System' (V2 CInt)
mapToSdlCoords mapTiles =
  worldToSdlCoords . Utils.modY (+1) . Utils.mapToWorldCoords mapTiles


scaleToCamera :: V2 CInt -> System' (V2 CInt)
scaleToCamera size = do
  Camera{ size_C = sizeC } <- get global
  let size' = liftA2 ((/) . fromIntegral) size sizeC ^/ Cons.coordsScale
  vpSize <- fmap fromIntegral <$> Renderer.getViewPortSize
  pure $ fmap ceiling $ vpSize ^^* size'


scaleRecToCamera :: Maybe (SDL.Rectangle CInt) 
                 -> System' (Maybe (SDL.Rectangle CInt))
scaleRecToCamera Nothing = pure Nothing
scaleRecToCamera (Just (SDL.Rectangle pos size)) = do
  size' <- scaleToCamera size
  pure . Just $ SDL.Rectangle pos size'


clampCameraToMap :: MapTiles -> Camera -> Camera
clampCameraToMap mapTiles cam@Camera{gameCoords_C = botLef, size_C = camSize} =
  let mapSize = V2 1 1 + (Utils.swapV . snd . Array.bounds) mapTiles
      topRig  = botLef + camSize
      botLefBound = V2 0 0
      topRigBound = fmap fromIntegral mapSize

      botLefDelta = liftA2 max botLef botLefBound - botLef
      topRigDelta = liftA2 min topRig topRigBound - topRig
  in cam { gameCoords_C = botLef + botLefDelta + topRigDelta }


{-
-- NOT USABLE YET
recCameraTransform :: Maybe (SDL.Rectangle CInt) 
                   -> System' (Maybe (SDL.Rectangle CInt))
recCameraTransform Nothing = pure Nothing
recCameraTransform (Just (SDL.Rectangle pos size)) = do
  size' <- scaleToCamera size


  pure Nothing
-}