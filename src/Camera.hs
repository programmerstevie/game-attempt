{-# LANGUAGE ScopedTypeVariables #-}
module Camera where


import ECS.Base
import qualified Constants as Cons
import qualified Utils
import qualified Renderer
import Utils ((^^*), (^^/))


import Apecs
import Control.Applicative
import Linear
import qualified SDL
import Foreign.C.Types (CFloat, CInt)



updateCamera :: System' ()
updateCamera = cmapM_ $ \(Player, aabb :: AABB) -> do
  scaleCoords <- coordsScale
  camCtr <- (^^/ scaleCoords) . fmap fromIntegral <$> Renderer.getViewPortSize
  global $~ \(camera :: Camera) -> 
    camera{ gameCoords_C = center_aabb aabb - camCtr ^/ 2 }
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
  return $ fmap floor $ Utils.modY (vpY -) $ camTrans ^^* scaledViewPort


mapToSdlCoords :: MapTiles -> V2 ICInt -> System' (V2 CInt)
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


{-
-- NOT USABLE YET
recCameraTransform :: Maybe (SDL.Rectangle CInt) 
                   -> System' (Maybe (SDL.Rectangle CInt))
recCameraTransform Nothing = pure Nothing
recCameraTransform (Just (SDL.Rectangle pos size)) = do
  size' <- scaleToCamera size


  pure Nothing
-}