{-# LANGUAGE OverloadedStrings #-}
module Main where

-- stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS"

import qualified Game
import ECS.Base (System', Running(..), initWorld)
import qualified Constants


import Apecs (runWith, runSystem, get, global)
import Control.Monad (when)
import SDL (V2(..))
import qualified SDL
import qualified SDL.Raw


main :: IO ()
main = do
  world <- initWorld
  runWith world $
    Game.init "THE BEST GAME EVER"
              (V2 SDL.Raw.SDL_WINDOWPOS_CENTERED 
                  SDL.Raw.SDL_WINDOWPOS_CENTERED)
              (V2 640 360)
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
    Game.update (1 / fromIntegral Constants.fps)
    Game.draw
    dT <- subtract frameStart <$> SDL.ticks
    when (Constants.frameDelay > dT) $
      SDL.delay (Constants.frameDelay - dT)
    gameLoop

-- TRY OUT LTS-16.5 FOR STACK
