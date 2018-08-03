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


data WeebAction = Normal | Dead deriving (Eq)


data Sprite = Sprite Dimensions Picture

type Frame = (Sprite,Float)

type Animation = [Frame]


data Sounds = Sounds 
            { sound1 :: String
            , sound2 :: String
            }

data Sprites = Sprites
            { torbStillRight    :: Sprite
            , torbStillLeft     :: Sprite
            , torbHammerRight   :: Sprite
            , torbHammerLeft    :: Sprite
            , torbRunRight      :: Sprite
            , torbRunLeft       :: Sprite
            , torbFallRight     :: Sprite
            , torbFallLeft      :: Sprite
            , weebStillRight    :: Sprite
            , weebStillLeft     :: Sprite
            , weebDabRight      :: Sprite
            , weebDabLeft       :: Sprite
            , weebSmackedRight1 :: Sprite
            , weebSmackedLeft1  :: Sprite
            , weebSmackedRight2 :: Sprite
            , weebSmackedLeft2  :: Sprite
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
               , gameEnded      :: Bool
               , soundQueue     :: [String]
               }

instance Show Weeb where
  show weeb = "Mada mada!" ++ (show $ weebPos weeb)