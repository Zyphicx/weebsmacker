module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when,unless,filterM,join)

import Data.Function (fix)
import Data.IORef
import Data.Time.Clock

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Rendering

import Graphics.UI.GLFW as GLFW

import System.Random

import Animation
import Backend
import Constants
import Game
import Rendering
import Types

-- Origin is always half initial width and height from left side and bottom side respectively

main :: IO ()
main = do
    glossState <- initState
    assets <- loadAssets
    time <- getCurrentTime
    timeRef <- newIORef time
    withWindow gameWidth gameHeight "Weeb smacker" $ \window -> do
        let loop state = do
              threadDelay (100000 `div` fps)
              pollEvents
              handleEvents window state

              timeDiff <- timeElapsed timeRef
              updateTimeRef timeRef
              state' <- (handleEvents window state >>= updateGameState timeDiff >>= updateAnimationStages timeDiff (snd assets))

              -- Networking goes here

              --putStrLn $ show $ hammerHitPoint state'
              --putStrLn $ show $ hammerAngle state

              --putStrLn $ show $ (playerVel . player) state
              --putStrLn $ show $ (playerPos . player) state
              --putStrLn $ show $ weebs state

              renderFrame window glossState assets state'

              let ended = gameEnded state'
              unless ended $ loop state'      
        loop initGameState
        terminate

-- Utility functions

-- Difficulties: Weak weebs, garbage genjis, shitty shimadas, noodle muchin narutos


timeElapsed :: IORef UTCTime -> IO NominalDiffTime
timeElapsed timeRef = do
    currentTime <- getCurrentTime
    oldTime <- readIORef timeRef
    return $ diffUTCTime currentTime oldTime

updateTimeRef :: IORef UTCTime -> IO ()
updateTimeRef timeRef = do
    currentTime <- getCurrentTime
    writeIORef timeRef currentTime