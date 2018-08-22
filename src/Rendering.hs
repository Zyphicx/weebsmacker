module Rendering where

import Data.Traversable (mapAccumL)

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Rendering

import Graphics.UI.GLFW as GLFW

import Constants
import Game
import Types

-- Rendering

playerPicture :: Sprites -> GameState -> Picture
playerPicture sprites state = sprite2Picture pic
  where
    pic = case (playerAction . player) state of
            (Running _)   -> firstFrame $ (animation . player) state
            (Falling _)   -> firstFrame $ (animation . player) state
            (Still Types.Right) -> torbStillRight sprites
            (Still Types.Left)  -> torbStillLeft sprites


weebPictures :: Sprites -> GameState -> Picture
weebPictures sprites state = Pictures $ zipWith moveWeebs (weebs state) $ fmap (sprite2Picture . pic) (weebs state)
  where
    pic weeb = case (weebAction weeb) of
                 Normal -> firstFrame $ weebAnimation weeb
                 Dead   -> firstFrame $ weebAnimation weeb

    moveWeebs (Weeb position _ _ _ _) weebPic = (uncurry translate) position weebPic


sprite2Picture :: Sprite -> Picture
sprite2Picture (Sprite (sWidth,sHeight) sprite) = translate (sWidth/2) (sHeight/2) sprite

spriteHeight :: Sprite -> Float
spriteHeight (Sprite (_,height) _) = height


renderFrame :: Window -> State -> (Sounds,Sprites) -> GameState -> IO ()
renderFrame window glossState (sounds,sprites) state = do
    (winWidth,winHeight) <- getWindowSize window

    let torbVer = playerPicture sprites state
    let torbScale  = torbHeight/torbSpriteHeight
    let torbSprite = scale torbScale torbScale torbVer
    let torbPos = (playerPos . player) state

    displayPicture (gameWidth,gameHeight) black glossState 1.0 $
      scale (fromIntegral winHeight / fromIntegral gameHeight) (fromIntegral winHeight / fromIntegral gameHeight) $ translate adjustX adjustY $
        Pictures 
          [ weebPictures sprites state
          , (uncurry translate) torbPos torbSprite
          , translateHammer state  $ sprite2Picture $ getTorbHammer sprites state
          , translate (fromIntegral gameWidth - (fromIntegral (lives state) + 1)*heartWidth) (fromIntegral gameHeight - heartHeight) $ showHearts sprites state
          , translate 0 (fromIntegral gameHeight - digitHeight) $ showPoints sprites state
          , translate (9 * digitWidth) (fromIntegral gameHeight - difficultyHeight) $ showDifficulty sprites state
          ]
    GLFW.swapBuffers window

showHearts :: Sprites -> GameState -> Picture
showHearts sprites state = Pictures $ heartList (fromIntegral (lives state))
  where 
    heartPic = sprite2Picture (heart sprites)
    heartList n
     | n == 0 = []
     | n > 0  = (translate (heartWidth*n) 0 heartPic):(heartList (n-1))
     | otherwise = []


showPoints :: Sprites -> GameState -> Picture
showPoints sprites state = number2Picture sprites (points state)

showDifficulty :: Sprites -> GameState -> Picture
showDifficulty sprites state = sprite2Picture $ difficulty2Sprite sprites (difficulty state)

difficulty2Sprite :: Sprites -> Difficulty -> Sprite
difficulty2Sprite sprites Easy    = (weakWeebs sprites)
difficulty2Sprite sprites Medium  = (garbageGenjis sprites)
difficulty2Sprite sprites Hard    = (shittyShimadas sprites)
difficulty2Sprite sprites Extreme = (noodleMunchinNarutos sprites)

number2Picture :: Sprites -> Int -> Picture
number2Picture sprites num = Pictures $ snd 
                                       $ mapAccumL (\acc x -> (acc+1,translate (acc*digitWidth) 0 x)) 0 
                                       $ fmap (sprite2Picture . char2Sprite sprites) 
                                       $ show num

char2Sprite :: Sprites -> Char -> Sprite
char2Sprite sprites '0' = digit0 sprites
char2Sprite sprites '1' = digit1 sprites
char2Sprite sprites '2' = digit2 sprites
char2Sprite sprites '3' = digit3 sprites
char2Sprite sprites '4' = digit4 sprites
char2Sprite sprites '5' = digit5 sprites
char2Sprite sprites '6' = digit6 sprites
char2Sprite sprites '7' = digit7 sprites
char2Sprite sprites '8' = digit8 sprites
char2Sprite sprites '9' = digit9 sprites


getTorbHammer :: Sprites -> GameState -> Sprite
getTorbHammer sprites state =
  case (playerFacing . player) state of
    Types.Right -> torbHammerRight sprites
    Types.Left -> torbHammerLeft sprites

translateHammer :: GameState -> Picture -> Picture
translateHammer state hammer = (uncurry translate) (hammerPosition state) $ rotate rotation hammer
  where
    rotation = case (playerFacing . player) state of
        Types.Right -> toDegrees (hammerMaxAngle - (hammerAngle state))
        Types.Left  -> toDegrees ((hammerAngle state) - hammerMaxAngle)


firstFrame :: (a,Animation) -> Sprite
firstFrame = fst . head . snd

toDegrees rad = rad*180/pi