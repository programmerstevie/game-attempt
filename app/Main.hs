{-# LANGUAGE OverloadedStrings #-}
module Main where

-- stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS"

import qualified Game
import ECS.Base (System', Running(..), initWorld)


import Apecs (runWith, runSystem, get, global)
import Control.Monad (when)
import Data.Word (Word32)
import SDL (V2(..))
import qualified SDL
import qualified SDL.Raw


fps, frameDelay :: Word32
fps = 30
frameDelay = 1000 `div` fps


main :: IO ()
main = do
  world <- initWorld
  runWith world $
    Game.init "GameEngine"
              (V2 SDL.Raw.SDL_WINDOWPOS_CENTERED 
                  SDL.Raw.SDL_WINDOWPOS_CENTERED)
              (V2 800 640)
              False
  runSystem gameLoop world
  runSystem Game.clean world
  putStrLn "Game Quit!"



gameLoop :: System' ()
gameLoop = do
  Running running <- get global
  when running $ do
    frameStart <- SDL.ticks
    Game.handleEvents
    Game.update
    Game.draw
    dT <- subtract frameStart <$> SDL.ticks
    when (frameDelay > dT) $
      SDL.delay (frameDelay - dT)
    gameLoop

-- TRY OUT LTS-16.5 FOR STACK