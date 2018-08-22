module Constants where

import Graphics.Gloss.Data.Color

import Types

gameWidth :: Int
gameWidth  = 1280

gameHeight :: Int
gameHeight = 720

adjustX,adjustY :: Float
adjustX = fromIntegral (-gameWidth)/2
adjustY = fromIntegral (-gameHeight)/2

torbHeightScale :: Float
torbHeightScale = 1/5

weebHeightScale :: Float
weebHeightScale = 1/7

moveSpeed :: Float
moveSpeed = fromIntegral gameWidth/2.5

hammerMaxAngle :: Float
hammerMaxAngle = atan(78/70)

attachAngleDiffRight :: Float
attachAngleDiffRight = atan(attachYRight/attachXRight) - hammerMaxAngle

attachXRight,attachYRight :: Float
attachXRight = 4
attachYRight = 25

attachRadiusRight :: Float
attachRadiusRight = sqrt (attachXRight ** 2 + attachYRight ** 2)

attachAngleDiffLeft :: Float
attachAngleDiffLeft = atan(attachYLeft/attachXLeft)

attachXLeft,attachYLeft :: Float
attachXLeft = 66
attachYLeft = 25

attachRadiusLeft :: Float
attachRadiusLeft = sqrt (attachXLeft ** 2 + attachYLeft ** 2)


swingSpeed :: Float
swingSpeed = hammerMaxAngle/0.15


hitPointAngleLeft :: Float
hitPointAngleLeft = pi/2

hitPointAngleRight :: Float
hitPointAngleRight = hammerMaxAngle

hitPointRadiusLeft :: Float
hitPointRadiusLeft = 78

hitPointRadiusRight :: Float
hitPointRadiusRight = sqrt (70 ** 2 + 78 ** 2)

minWeebCooldown :: Difficulty -> Float
minWeebCooldown Easy    = 1.5
minWeebCooldown Medium  = 1
minWeebCooldown Hard    = 0.8
minWeebCooldown Extreme = 0.6

maxWeebCooldown :: Difficulty -> Float
maxWeebCooldown Easy    = 3
maxWeebCooldown Medium  = 2
maxWeebCooldown Hard    = 2
maxWeebCooldown Extreme = 1.5


digitWidth,digitHeight :: Float
digitWidth = 30
digitHeight = 40

heartWidth,heartHeight :: Float
heartWidth = 40
heartHeight = 40

difficultyHeight :: Float
difficultyHeight = 40


initGameState = GameState (Player (0,0) 0 Types.Left [] (Still Types.Left) (Still Types.Left,[])) [] hammerMaxAngle Nothing 0 0 0 7 Extreme 0 False []

backgroundColor = makeColor 204 255 255 255

fps :: Int
fps = 60

jumpSpeed :: Float
jumpSpeed = 1200*(fromIntegral gameHeight)

g :: Float
g = (-2)*(fromIntegral gameHeight)

deadTimer :: Float
deadTimer = 1

aliveTimer :: Float
aliveTimer = 10

torbSpriteWidth,torbSpriteHeight :: Float
torbSpriteWidth = 125
torbSpriteHeight = 208

torbRatio = torbSpriteWidth/torbSpriteHeight

torbWidth,torbHeight :: Float
torbWidth = torbHeight * torbRatio
torbHeight = (fromIntegral gameHeight) * torbHeightScale


weebSpriteWidth,weebSpriteHeight :: Float
weebSpriteWidth = 120
weebSpriteHeight = 129

weebRatio = weebSpriteWidth/weebSpriteHeight

weebWidth,weebHeight :: Float
weebWidth = weebHeight * weebRatio
weebHeight = (fromIntegral gameHeight) * weebHeightScale