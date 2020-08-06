module TextureManager where
-- import qualified


import ECS.Base


import Foreign.C.Types (CInt)
import Apecs
import Control.Monad (unless)
import Data.Aeson
import Data.Maybe
import Linear
import qualified Data.HashMap.Strict as HM
import qualified SDL
import qualified SDL.Image as IMG


loadAnimationMap :: FilePath -> System' ()
loadAnimationMap fp = do
  am <- fmap unAnimationMap <$> liftIO (decodeFileStrict' fp)
  let animMap = fromMaybe (error "Animation could not be loaded.") am
  loadTextures . map filePath_A $ HM.elems animMap
  global $~ \(AnimationMap animations) -> 
    AnimationMap (HM.union animMap animations)


getTexturePure :: FilePath -> HM.HashMap FilePath SDL.Texture -> SDL.Texture
getTexturePure path texMap =
  HM.lookupDefault (texMap HM.! "assets/DEFAULT.png") path texMap


getTexture :: FilePath -> System' SDL.Texture
getTexture path = getTexturePure path . unTextures <$> get global


loadTexture :: FilePath -> System' SDL.Texture
loadTexture path = do
  Textures texMap <- get global
  unless (HM.member path texMap) $ do
    CRenderer renderer <- get global
    tempSurface <- IMG.load path
    tex <- SDL.createTextureFromSurface renderer tempSurface
    SDL.freeSurface tempSurface
    global $= Textures (HM.insert path tex texMap)
  (HM.! path) . unTextures <$> get global


loadTextures :: [FilePath] -> System' [SDL.Texture]
loadTextures = traverse loadTexture


draw :: SDL.Texture
     -> Maybe (SDL.Rectangle CInt)
     -> Maybe (SDL.Rectangle CInt)
     -> V2 Bool
     -> System' ()
draw tex src dest flp = do
  renderer <- unRenderer <$> get global
  SDL.copyEx renderer tex src dest 0 Nothing flp

