{-# LANGUAGE TypeFamilies #-}
module ECS.PlayerComponents where


import Apecs.Core (Component, Storage)
import Apecs.Stores (Unique)


data Player = Player

instance Component Player where
  type Storage Player = Unique Player


newtype FastFalling = FastFalling Bool

instance Component FastFalling where
  type Storage FastFalling = Unique FastFalling