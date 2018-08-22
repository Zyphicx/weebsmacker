module Types where

import Graphics.Gloss.Data.Picture
import Graphics.UI.GLFW as GLFW

-- Types

type Dimensions = (Float,Float)

type Position = (Float,Float)

data Direction = Left | Right deriving (Eq)

data Action = Running Direction | Still Direction | Falling Direction deriving (Eq)

data Difficulty = Easy | Medium | Hard | Extreme

data SwingDirection = SwingUp | SwingDown deriving (Eq)

data Sound = SoundHit | SoundLose | SoundDisappear


data WeebAction = Normal | Dead deriving (Eq)


data Sprite = Sprite Dimensions Picture

type Frame = (Sprite,Float)

type Animation = [Frame]


data Sounds = Sounds 
            { sound1 :: String
            , sound2 :: String
            }

data Sprites = Sprites
            { torbStillRight       :: Sprite
            , torbStillLeft        :: Sprite
            , torbHammerRight      :: Sprite
            , torbHammerLeft       :: Sprite
            , torbRunRight         :: Sprite
            , torbRunLeft          :: Sprite
            , torbFallRight        :: Sprite
            , torbFallLeft         :: Sprite
            , weebStillRight       :: Sprite
            , weebStillLeft        :: Sprite
            , weebDabRight         :: Sprite
            , weebDabLeft          :: Sprite
            , weebSmackedRight1    :: Sprite
            , weebSmackedLeft1     :: Sprite
            , weebSmackedRight2    :: Sprite
            , weebSmackedLeft2     :: Sprite
            , heart                :: Sprite
            , digit0               :: Sprite
            , digit1               :: Sprite
            , digit2               :: Sprite
            , digit3               :: Sprite
            , digit4               :: Sprite
            , digit5               :: Sprite
            , digit6               :: Sprite
            , digit7               :: Sprite
            , digit8               :: Sprite
            , digit9               :: Sprite
            , weakWeebs            :: Sprite
            , garbageGenjis        :: Sprite
            , shittyShimadas       :: Sprite
            , noodleMunchinNarutos :: Sprite
            }

data Player = Player 
              { playerPos     :: Position
              , playerVel     :: Float
              , playerFacing  :: Direction
              , pressedKeys   :: [Key]
              , playerAction  :: Action
              , animation     :: (Action,Animation)
              }

data Weeb = Weeb
          { weebPos       :: Position
          , weebAnimation :: (WeebAction,Animation)
          , weebAction    :: WeebAction
          , weebDead      :: Float
          , weebAlive     :: Float
          }
data GameState = GameState 
               { player         :: Player
               , weebs          :: [Weeb]
               , hammerAngle    :: Float
               , swingDirection :: Maybe SwingDirection
               , swingCooldown  :: Float
               , weebCooldown   :: Float
               , points         :: Int
               , lives          :: Int
               , difficulty     :: Difficulty
               , diffCooldown   :: Float
               , gameEnded      :: Bool
               , soundQueue     :: [Sound]
               }

instance Show Weeb where
  show weeb = "Mada mada!" ++ (show $ weebPos weeb)