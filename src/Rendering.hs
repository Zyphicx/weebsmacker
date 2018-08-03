module Rendering where

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

    moveWeebs (Weeb position _ _ _) weebPic = (uncurry translate) position weebPic


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
          , translate ($ hearts
          ]
    GLFW.swapBuffers window

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