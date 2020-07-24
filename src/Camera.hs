{-# LANGUAGE ScopedTypeVariables #-}
module Camera where

import ECS.Base
import qualified Constants as Cons
import qualified Utils
import qualified Renderer
import Utils ((^^*), (^^/))

import Apecs
import Linear
import qualified SDL
import Foreign.C.Types (CFloat, CInt)


worldToCamera :: Camera -> V2 CFloat -> V2 CFloat
worldToCamera Camera{ gameCoords_C = coords_C} coords = coords - coords_C


worldToSdlCoords :: V2 CFloat -> System' (V2 CInt)
worldToSdlCoords coords = do
  Camera{ gameCoords_C = coordsC, size_C = sizeC } <- get global
  viewPortSize@(V2 _ vpY) <- Renderer.getViewPortSize
  let camTrans = coords - coordsC
      scaledViewPort = fmap fromIntegral viewPortSize ^^/ sizeC
  return $ Utils.modY (vpY -) . fmap floor $ camTrans ^^* scaledViewPort


mapToSdlCoords :: MapTiles -> V2 CInt -> System' (V2 CInt)
mapToSdlCoords mapTiles = 
  worldToSdlCoords . Utils.modY (+1) . Utils.mapToWorldCoords mapTiles


scaleToCamera :: V2 CInt -> System' (V2 CInt)
scaleToCamera size = do
  Camera{ size_C = sizeC } <- get global
  let size' = (fmap fromIntegral size ^^/ sizeC) ^/ Cons.coordsScale
  vpSize <- fmap fromIntegral <$> Renderer.getViewPortSize
  pure $ fmap ceiling $ vpSize ^^* size'


scaleRecToCamera :: Maybe (SDL.Rectangle CInt) 
                 -> System' (Maybe (SDL.Rectangle CInt))
scaleRecToCamera Nothing = pure Nothing
scaleRecToCamera (Just (SDL.Rectangle pos size)) = do
  size' <- scaleToCamera size
  pure . Just $ SDL.Rectangle pos size'



-- NOT USABLE YET
recCameraTransform :: Maybe (SDL.Rectangle CInt) 
                   -> System' (Maybe (SDL.Rectangle CInt))
recCameraTransform Nothing = pure Nothing
recCameraTransform (Just (SDL.Rectangle pos size)) = do
  size' <- scaleToCamera size


  pure Nothing
