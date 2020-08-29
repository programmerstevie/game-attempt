{-# LANGUAGE ScopedTypeVariables #-}
module Player where


import ECS.Base
import qualified Collisions

import Apecs
import Control.Monad (when)

updatePlayer :: System' ()
updatePlayer =
  cmapM_ $ \(Player, aabb_p :: AABB) ->
    cmapM_ $ \(Dinosaur, aabb_d :: AABB, ety :: Entity) ->
      when (Collisions.overlaps aabb_p aabb_d) $
        ety $= Active False