module ECS.OrphanInstances where


import Data.Aeson
import Data.Ix
import Foreign.C.Types
import Linear
import qualified Data.Hashable as HASH
import qualified Data.Scientific as SCI
import qualified Data.Vector as V

instance HASH.Hashable CInt where
  hashWithSalt i (CInt v) = HASH.hashWithSalt i v 
  
instance Ix CInt where
  range (m,n) = [m..n]
  index b@(m,_n) i | inRange b i = fromIntegral $ i - m
                   | otherwise   = error "CInt index Error"
  inRange (m, n) i =  (m <= i) && (i <= n)

instance FromJSON CFloat where
  parseJSON = withScientific "CFloat" $ pure . SCI.toRealFloat

instance FromJSON CInt where
  parseJSON = withScientific "CInt" $ \n ->
    case SCI.toBoundedInteger n of
      Just v  -> pure v
      Nothing -> error "nonInteger for CInt from JSON!"

instance (FromJSON a) => FromJSON (V2 a) where
  parseJSON = withArray "V2" $ \v ->
    if V.length v == 2
    then do
      x <- parseJSON (v V.! 0)
      y <- parseJSON (v V.! 1)
      pure $ V2 x y
    else error "array of length 2 trying to be converted to V2"