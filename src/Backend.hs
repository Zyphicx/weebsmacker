module Backend where

import Control.Monad (when,filterM)

import Graphics.Gloss.Rendering
import Graphics.UI.GLFW as GLFW

import Constants
import Types

-- Handle events

handleEvents :: Window -> GameState -> IO GameState
handleEvents win state = do
    keys <- filterM (keyFilter win) 
      [ Key'W
      , Key'A
      , Key'S
      , Key'D
      , Key'Space
      , Key'Escape
      , Key'Up
      , Key'Down
      ]

    keyStates <- sequence $ fmap (getKey win) keys
    --putStrLn $ show keyStates
    return $ state { player = (player state) { pressedKeys = keys } }
      where
        keyFilter win x = fmap isPress $ getKey win x


-- Load assets

loadAssets :: IO (Sounds,Sprites)
loadAssets = (,) <$> loadSounds <*> loadSprites

loadSounds :: IO Sounds
loadSounds = return $ Sounds "Beep" "Boop"

loadSprites :: IO Sprites
loadSprites = do
    p1  <- loadBMP $ base ++ "TorbStillRight.bmp"
    p2  <- loadBMP $ base ++ "TorbStillLeft.bmp"
    p3  <- loadBMP $ base ++ "TorbHammerRight.bmp"
    p4  <- loadBMP $ base ++ "TorbHammerLeft.bmp"
    p5  <- loadBMP $ base ++ "TorbRunRight1.bmp"
    p6  <- loadBMP $ base ++ "TorbRunLeft1.bmp"
    p7  <- loadBMP $ base ++ "TorbFallRight.bmp"
    p8  <- loadBMP $ base ++ "TorbFallLeft.bmp"
    p9  <- loadBMP $ base ++ "WeebStillRight.bmp"
    p10 <- loadBMP $ base ++ "WeebStillLeft.bmp"
    p11 <- loadBMP $ base ++ "WeebDabRight.bmp"
    p12 <- loadBMP $ base ++ "WeebDabLeft.bmp"
    p13 <- loadBMP $ base ++ "WeebSmackedRight1.bmp"
    p14 <- loadBMP $ base ++ "WeebSmackedLeft1.bmp"
    p15 <- loadBMP $ base ++ "WeebSmackedRight2.bmp"
    p16 <- loadBMP $ base ++ "WeebSmackedLeft2.bmp"
    p17 <- loadBMP $ base ++ "heart.bmp"
    p18 <- loadBMP $ base ++ "0.bmp"
    p19 <- loadBMP $ base ++ "1.bmp"
    p20 <- loadBMP $ base ++ "2.bmp"
    p21 <- loadBMP $ base ++ "3.bmp"
    p22 <- loadBMP $ base ++ "4.bmp"
    p23 <- loadBMP $ base ++ "5.bmp"
    p24 <- loadBMP $ base ++ "6.bmp"
    p25 <- loadBMP $ base ++ "7.bmp"
    p26 <- loadBMP $ base ++ "8.bmp"
    p27 <- loadBMP $ base ++ "9.bmp"
    p28 <- loadBMP $ base ++ "WeakWeebs.bmp"
    p29 <- loadBMP $ base ++ "GarbageGenjis.bmp"
    p30 <- loadBMP $ base ++ "ShittyShimadas.bmp"
    p31 <- loadBMP $ base ++ "NoodleMunchinNarutos.bmp"
    return $ 
      Sprites 
        (Sprite (torbSpriteWidth,torbSpriteHeight) p1) 
        (Sprite (torbSpriteWidth,torbSpriteHeight) p2) 
        (Sprite (71,98) p3) 
        (Sprite (71,98) p4)
        (Sprite (torbSpriteWidth,torbSpriteHeight) p5)
        (Sprite (torbSpriteWidth,torbSpriteHeight) p6)
        (Sprite (torbSpriteWidth,torbSpriteHeight) p7)
        (Sprite (torbSpriteWidth,torbSpriteHeight) p8)
        (Sprite (weebSpriteWidth, weebSpriteHeight) p9)
        (Sprite (weebSpriteWidth, weebSpriteHeight) p10)
        (Sprite (weebSpriteWidth, weebSpriteHeight) p11)
        (Sprite (weebSpriteWidth, weebSpriteHeight) p12)
        (Sprite (120,90) p13)
        (Sprite (120,90) p14)
        (Sprite (120,100) p15)
        (Sprite (120,100) p16)
        (Sprite (heartWidth,heartHeight) p17)
        (Sprite (digitWidth,digitHeight) p18)
        (Sprite (digitWidth,digitHeight) p19)
        (Sprite (digitWidth,digitHeight) p20)
        (Sprite (digitWidth,digitHeight) p21)
        (Sprite (digitWidth,digitHeight) p22)
        (Sprite (digitWidth,digitHeight) p23)
        (Sprite (digitWidth,digitHeight) p24)
        (Sprite (digitWidth,digitHeight) p25)
        (Sprite (digitWidth,digitHeight) p26)
        (Sprite (digitWidth,digitHeight) p27)
        (Sprite (269,difficultyHeight) p28)
        (Sprite (292,difficultyHeight) p29)
        (Sprite (261,difficultyHeight) p30)
        (Sprite (371,difficultyHeight) p31)

      where
          base = "assets/sprites/"

exitPressed :: Window -> IO Bool
exitPressed window = fmap isPress $ GLFW.getKey window Key'Escape

isPress :: KeyState -> Bool
isPress KeyState'Pressed = True
isPress _                = False

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO () 
withWindow width height title f = do 
      GLFW.setErrorCallback $ Just simpleErrorCallback 
      r <- GLFW.init 
      when r $ do 
        m <- GLFW.createWindow width height title Nothing Nothing 
        case m of 
          (Just win) -> do 
            GLFW.makeContextCurrent m 
            f win 
            GLFW.setErrorCallback $ Just simpleErrorCallback 
            GLFW.destroyWindow win 
          Nothing -> return () 
      GLFW.terminate 
  where 
    simpleErrorCallback e s = putStrLn $ unwords [show e, show s]