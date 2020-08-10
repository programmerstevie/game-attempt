module ECS.Utils where


import Foreign.C.Types (CInt)
import Linear
import qualified SDL

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


