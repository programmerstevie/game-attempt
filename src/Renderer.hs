{-# LANGUAGE ScopedTypeVariables #-}
module Renderer where


import ECS.Base
import qualified Constants as Cons


import Apecs
import Linear
import qualified SDL
import qualified Utils
import Foreign.C.Types (CInt, CFloat)




setFullViewPort :: System' ()
setFullViewPort = 
  (SDL.rendererViewport . unRenderer <$> get global) >>= (SDL.$= Nothing)


setCamViewPort :: System' ()
setCamViewPort = do
  (cam :: Camera, CRenderer renderer) <- get global
  winSize <- getWindowSizeAbsolute
  SDL.rendererViewport renderer SDL.$= getViewPort winSize cam


getViewPort :: V2 CInt -> Camera -> Maybe (SDL.Rectangle CInt)
getViewPort winSize cam =
  let V2 cX cY = size_C cam
      V2 winX winY = fmap fromIntegral winSize
      camXtoY = cX / cY
      winXtoY = winX / winY
      (w, h, x, y) = 
        if winXtoY > camXtoY
        then (winY * cX / cY, winY, (winX - (winY * cX / cY)) / 2, 0)
        else (winX, winX * cY / cX, 0, (winY - (winX * cY / cX)) / 2)
  in Just . fmap floor $ SDL.Rectangle (SDL.P $ V2 x y) $ V2 w h


getWindowSizeWorld :: System' (V2 CFloat)
getWindowSizeWorld = 
  (^/ Cons.coordsScale) . fmap fromIntegral <$> getWindowSizeAbsolute


getWindowSizeAbsolute :: System' (V2 CInt)
getWindowSizeAbsolute = SDL.get . SDL.windowSize . unWindow =<< get global


getViewPortSize :: System' (V2 CInt)
getViewPortSize = do
  renderer <- unRenderer <$> get global
  Utils.getRectSize =<< SDL.get (SDL.rendererViewport renderer)