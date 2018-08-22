module Animation where

import Data.Time.Clock
import Types

-- Animations

torbRunRightAnimation :: Sprites -> Animation
torbRunRightAnimation s = [(torbRunRight s,0.2), (torbStillRight s,0.2)]

torbRunLeftAnimation :: Sprites -> Animation
torbRunLeftAnimation s = [(torbRunLeft s,0.2), (torbStillLeft s,0.2)]

torbFallRightAnimation :: Sprites -> Animation
torbFallRightAnimation s = [(torbFallRight s,0.2), (torbStillRight s,0.2)]

torbFallLeftAnimation :: Sprites -> Animation
torbFallLeftAnimation s = [(torbFallLeft s,0.2), (torbStillLeft s,0.2)]

weebDabAnimation :: Sprites -> Animation
weebDabAnimation s = [(weebStillLeft s,2.0), (weebDabLeft s,0.2)]

weebDeathAnimation :: Sprites -> Animation
weebDeathAnimation s = [(weebSmackedLeft1 s,0.3), (weebSmackedLeft2 s,0.3)]

-- Update animation stages

updateAnimationStages :: NominalDiffTime -> Sprites -> GameState -> IO GameState
updateAnimationStages timeDiff sprites state = return $ updateWeebAnimations timeDiff sprites $ updatePlayerAnimation timeDiff sprites state

updatePlayerAnimation :: NominalDiffTime -> Sprites -> GameState -> GameState
updatePlayerAnimation timeDiff sprites state = state { player = (player state) { animation = animation' } }
  where 
    action = (playerAction . player) state
    curAnimation = (animation . player) state

    animation' = 
      if action == fst curAnimation
        then stepAnimation curAnimation
        else (action, getPlayerAnimation sprites action)

    stepAnimation :: (Action,Animation) -> (Action,Animation)
    stepAnimation (prevAction,((sprite,duration)):xs) = 
        let duration' = duration - (realToFrac timeDiff)
        in if duration' > 0 
              then (prevAction,((sprite,duration'):xs)) 
              else case xs of
                    [] -> (prevAction,getPlayerAnimation sprites prevAction)
                    _  -> (prevAction,xs)
    stepAnimation (_,[]) = (Still Types.Left,[])

updateWeebAnimations :: NominalDiffTime -> Sprites -> GameState -> GameState
updateWeebAnimations timeDiff sprites state = state { weebs = fmap (updateWeebAnimation timeDiff sprites) $ weebs state }

updateWeebAnimation :: NominalDiffTime -> Sprites -> Weeb -> Weeb
updateWeebAnimation timeDiff sprites (Weeb p animation action dead alive) = Weeb p animation' action dead alive
  where
    animation' =
      if null $ snd animation
        then
          (action, getWeebAnimation sprites action)
        else
          if fst animation == action 
            then stepAnimation animation
            else (action, getWeebAnimation sprites action)

    stepAnimation :: (WeebAction,Animation) -> (WeebAction,Animation)
    stepAnimation (prevAction,((sprite,duration)):xs) =
        let duration' = duration - (realToFrac timeDiff)
        in if duration' > 0
             then (prevAction,((sprite,duration'):xs))
             else case xs of
                [] -> (prevAction,getWeebAnimation sprites prevAction)
                _  -> (prevAction,xs)
    stepAnimation (_,[]) = (Normal,[])


getPlayerAnimation :: Sprites -> Action -> Animation
getPlayerAnimation sprites (Running Types.Left)  = torbRunLeftAnimation sprites
getPlayerAnimation sprites (Running Types.Right) = torbRunRightAnimation sprites
getPlayerAnimation sprites (Falling Types.Left)  = torbFallLeftAnimation sprites
getPlayerAnimation sprites (Falling Types.Right) = torbFallRightAnimation sprites
getPlayerAnimation _ _                           = []

getWeebAnimation :: Sprites -> WeebAction -> Animation
getWeebAnimation sprites Normal = weebDabAnimation sprites
getWeebAnimation sprites Dead   = weebDeathAnimation sprites