module Game where

import Data.Time.Clock

import Graphics.UI.GLFW as GLFW

import System.Random

import Constants
import Types

-- Hammer position

hammerPosition :: GameState -> (Float,Float)
hammerPosition state = (x+dx,y+dy)
  where
    (x,y) = (playerPos . player) state
    (dx,dy) = case (playerFacing . player) state of 
                Types.Right -> ( attachXRight - attachRadiusRight*cos(hammerAngle state + attachAngleDiffRight) + torbWidth/2 - torbWidth/11
                               , attachYRight - attachRadiusRight*sin(hammerAngle state + attachAngleDiffRight) + torbHeight/2)
                Types.Left  -> ( attachXLeft - attachRadiusLeft*cos(hammerMaxAngle - hammerAngle state + attachAngleDiffLeft) - torbWidth/2 + torbWidth/4
                               , attachYLeft - attachRadiusLeft*sin(hammerMaxAngle - hammerAngle state + attachAngleDiffLeft) + torbHeight/2)

hammerHitPoint :: GameState -> (Float,Float)
hammerHitPoint state = (x+dx,y+dy)
  where
    (x,y) = hammerPosition state
    (dx,dy) = case (playerFacing . player) state of
                Types.Right -> ( hitPointRadiusRight*cos(hammerAngle state)
                               , hitPointRadiusRight*sin(hammerAngle state))
                Types.Left  -> ( hitPointRadiusLeft*cos(hammerMaxAngle - (hammerAngle state) + hitPointAngleLeft)
                               , hitPointRadiusLeft*sin(hammerMaxAngle - (hammerAngle state) + hitPointAngleLeft))


isInHitbox :: Position -> Weeb -> Bool
isInHitbox (px,py) weeb = (px >= minX && px <= maxX) && (py >= minY && py <= maxY)
  where
    (minX,minY) = weebPos weeb
    maxX = minX + weebWidth
    maxY = minY + weebHeight

-- Update game state

updateGameState :: NominalDiffTime -> GameState -> IO GameState
updateGameState timeDiff state = (return $
                               ( updateDiffCooldown timeDiff
                               . changeDifficulty
                               . killWeebs
                               . clearWeebs
                               . updateWeebTimers timeDiff
                               . hammerSmack 
                               . moveHammer timeDiff
                               . groundVelocityReset 
                               . outOfMap
                               . moveVertical timeDiff 
                               . jump 
                               . fall timeDiff 
                               . exitGame
                               . standStill
                               . swingHammer
                               . updateHammerCooldown timeDiff
                               . moveLeft timeDiff 
                               . moveRight timeDiff ) state) >>= spawnWeebs timeDiff

moveLeft :: NominalDiffTime -> GameState -> GameState
moveLeft timeDiff state = if keyPressed
                            then state { player = (player state) { playerPos = (x+dx,y), playerFacing = Types.Left, playerAction = playerAction' } }  
                            else state
  where
    (x,y) = (playerPos . player) state
    onGround = y <= 0

    playerAction' = if onGround then Running Types.Left else (playerAction . player) state

    keyPressed = elem Key'A $ (pressedKeys . player) state
    dx = (-1)*moveSpeed*(realToFrac timeDiff)

moveRight :: NominalDiffTime -> GameState -> GameState
moveRight timeDiff state = if keyPressed
                             then state { player = (player state) { playerPos = (x+dx,y), playerFacing = Types.Right, playerAction = playerAction' } } 
                             else state
  where
    (x,y) = (playerPos . player) state
    onGround = y <= 0

    playerAction' = if onGround then Running Types.Right else (playerAction . player) state

    keyPressed = elem Key'D $ (pressedKeys . player) state
    dx = 1*moveSpeed*(realToFrac timeDiff)

standStill :: GameState -> GameState
standStill state = if keyPressed && onGround
                    then state { player = (player state) { playerAction = Still $ (playerFacing . player) state } }
                    else state
  where
    (_,y) = (playerPos . player) state
    onGround = y <= 0
    keys = (pressedKeys . player) state
    keyPressed = (not $ elem Key'D keys) && (not $ elem Key'A keys)

groundVelocityReset :: GameState -> GameState
groundVelocityReset state
  | onGround            = state { player = (player state) { playerVel = 0 } }
  | inRoof && jumping   = state { player = (player state) { playerVel = 0 } }
  | otherwise           = state
  where
    (_,y) = (playerPos . player) state
    vel = (playerVel . player) state
    jumping = vel > 0
    onGround = y <= 0
    inRoof = y >= fromIntegral (gameHeight - round torbHeight)

jump :: GameState -> GameState
jump state = if keyPressed && onGround then state { player = (player state) { playerVel = vel' } } else state
  where
    keyPressed = elem Key'W $ (pressedKeys . player) state
    (_,y) = (playerPos . player) state
    onGround = y <= 0
    vel = (playerVel . player) state
    vel' = vel + (realToFrac jumpSpeed)/(realToFrac gameHeight)

fall :: NominalDiffTime -> GameState -> GameState
fall timeDiff state = state { player = (player state) { playerVel = vel' } }
  where
    (_,y) = (playerPos . player) state
    onGround = y <= 0
    vel = (playerVel . player) state
    dv = if not onGround then g*(realToFrac timeDiff) else 0
    vel' = dv + vel

moveVertical :: NominalDiffTime -> GameState -> GameState
moveVertical timeDiff state = state { player = (player state) { playerPos = pos', playerAction = playerAction' } }
  where
    (x,y) = (playerPos . player) state
    vel = (playerVel . player) state

    playerAction' = getAction vel

    dy = vel*(realToFrac timeDiff)
    pos' = (x,y+dy)
    
    getAction vel
      | vel < 0   = Falling $ (playerFacing . player) state
      | vel > 0   = Running $ (playerFacing . player) state
      | otherwise = (playerAction . player) state

outOfMap :: GameState -> GameState
outOfMap state
  | x < 0          = state { player = (player state) { playerPos = (0,y) } }
  | x > rightBound = state { player = (player state) { playerPos = (rightBound,y) } }
  | y < 0          = state { player = (player state) { playerPos = (x,0) } }
  | y > topBound   = state { player = (player state) { playerPos = (x,topBound) } }
  | otherwise      = state
  where
    (x,y) = (playerPos . player) state
    rightBound = fromIntegral (gameWidth - round torbWidth) :: Float
    topBound = fromIntegral (gameHeight - round torbHeight) :: Float


updateHammerCooldown :: NominalDiffTime -> GameState -> GameState
updateHammerCooldown timeDiff state = if (swingDirection state == Nothing) then state { swingCooldown = swingCooldown' } else state
  where
    cooldown = (swingCooldown state) - (realToFrac timeDiff)
    swingCooldown' = if cooldown >= 0 then cooldown else 0


updateDiffCooldown :: NominalDiffTime -> GameState -> GameState
updateDiffCooldown timeDiff state = state { diffCooldown = diffCooldown' }
  where
    cooldown = (diffCooldown state) - (realToFrac timeDiff)
    diffCooldown' = if cooldown >= 0 then cooldown else 0

swingHammer :: GameState -> GameState
swingHammer state = if keyPressed && (swingDirection state == Nothing) && (swingCooldown state == 0) then state { swingDirection = Just SwingDown, swingCooldown = 0.4 } else state
  where
    keyPressed = elem Key'Space $ (pressedKeys . player) state

moveHammer :: NominalDiffTime -> GameState -> GameState
moveHammer timeDiff state = state { hammerAngle = angle', swingDirection = swingDirection' }
  where
    angle = hammerAngle state
    dAngle =
      case swingDirection state of
        (Just SwingUp)   -> swingSpeed*(realToFrac timeDiff)
        (Just SwingDown) -> (-swingSpeed)*(realToFrac timeDiff)
        Nothing          -> 0

    newAngle = angle + dAngle

    (swingDirection',angle') = 
      case swingDirection state of
        (Just SwingUp)   -> if newAngle >= hammerMaxAngle then (Nothing, hammerMaxAngle) else (Just SwingUp, newAngle)
        (Just SwingDown) -> if newAngle <= 0 then (Just SwingUp, (-newAngle)) else (Just SwingDown, newAngle)
        Nothing -> (Nothing, hammerMaxAngle)


hammerSmack :: GameState -> GameState
hammerSmack state = if swingDirection state == Just SwingDown then state' else state
  where
    smackWeeb weeb@(Weeb pos animation action dead alive) =
        if (hammerHitPoint state) `isInHitbox` weeb && (weebAction weeb /= Dead)
          then
            Weeb pos animation Dead deadTimer alive
          else
            weeb

    newState = state { weebs = fmap smackWeeb (weebs state) }

    dPoints = length $ filter (\(Weeb _ _ action dead _) -> action == Dead && dead == deadTimer) (weebs newState)

    state' = newState { points = (points newState) + dPoints}

updateWeebTimers :: NominalDiffTime -> GameState -> GameState
updateWeebTimers timeDiff = updateWeebDeadTimer timeDiff . updateWeebAliveTimer timeDiff

updateWeebDeadTimer :: NominalDiffTime -> GameState -> GameState
updateWeebDeadTimer timeDiff state = state { weebs = fmap (weebTimer) (weebs state) }
  where
   weebTimer weeb@(Weeb pos animation action dead alive) = 
    if action == Dead
      then Weeb pos animation action (dead - (realToFrac timeDiff)) alive
      else weeb

updateWeebAliveTimer :: NominalDiffTime -> GameState -> GameState
updateWeebAliveTimer timeDiff state = state { weebs = fmap (weebTimer) (weebs state) }
  where
   weebTimer weeb@(Weeb pos animation action dead alive) = 
    if action == Normal
      then Weeb pos animation action dead (alive - (realToFrac timeDiff))
      else weeb

killWeebs :: GameState -> GameState
killWeebs state = state { weebs = filter (\(Weeb _ _ _ _ alive) -> alive > 0) (weebs state), lives = (lives state - toKill) }
  where
    toKill = length $ filter (\(Weeb _ _ _ _ alive) -> alive < 0) (weebs state)

clearWeebs :: GameState -> GameState
clearWeebs state = state { weebs = filter (\(Weeb _ _ _ dead _) -> dead > 0) (weebs state) } 

changeDifficulty :: GameState -> GameState
changeDifficulty state = if diffCooldown state <= 0 then state { difficulty = difficulty', diffCooldown = 0.1 } else state
  where
    keysPressed = filter (\key -> key == Key'Up || key == Key'Down) $ (pressedKeys . player) state

    difficulty' = 
        if null keysPressed 
            then difficulty state 
            else (keyToCycleFunc $ head keysPressed) (difficulty state)

    keyToCycleFunc Key'Up   = cycleDifficultyUp
    keyToCycleFunc Key'Down = cycleDifficultyDown

cycleDifficultyUp :: Difficulty -> Difficulty
cycleDifficultyUp Easy    = Medium
cycleDifficultyUp Medium  = Hard
cycleDifficultyUp Hard    = Extreme
cycleDifficultyUp Extreme = Easy

cycleDifficultyDown :: Difficulty -> Difficulty
cycleDifficultyDown Easy    = Extreme
cycleDifficultyDown Medium  = Easy
cycleDifficultyDown Hard    = Medium
cycleDifficultyDown Extreme = Hard

spawnWeebs :: NominalDiffTime -> GameState -> IO GameState
spawnWeebs timeDiff state = do
  let cooldown' = (weebCooldown state) - (realToFrac timeDiff)
  x <- getStdRandom (randomR (minX,maxX))
  y <- getStdRandom (randomR (minY,maxY))
  newCoolDown <- getStdRandom (randomR (minWeebCooldown $ difficulty state, maxWeebCooldown $ difficulty state))

  let weebs' = if cooldown' < 0 then (Weeb (fromIntegral x, fromIntegral y) (Normal,[]) Normal deadTimer aliveTimer):(weebs state) else weebs state

  return $ state { weebs = weebs', weebCooldown = if cooldown' < 0 then newCoolDown else cooldown' }
    where
      minX = 0
      minY = 0
      maxX = gameWidth - (round weebWidth)
      maxY = gameHeight - (round weebHeight)


exitGame :: GameState -> GameState
exitGame state = state { gameEnded = ended }
  where
    keys = (pressedKeys . player) state
    ended = if (Key'Escape `elem` keys) then True else False