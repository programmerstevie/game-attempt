module Constants where


import Foreign.C.Types (CInt, CFloat)
import Linear


minJumpSpeed :: CFloat
minJumpSpeed = 4


playerJumpHeight, playerFallTime, playerJumpSpeed :: CFloat
playerJumpHeight = 5
playerFallTime = 0.2
playerJumpSpeed = sqrt $ 2 * playerJumpHeight * abs gravity

gravity :: CFloat
gravity = -2 * playerJumpHeight / (playerFallTime ** 2)


friction :: CFloat
friction = -160



correctWalkFriction :: CFloat
correctWalkFriction = -300


airFriction :: CFloat
airFriction = -100


coordsScale :: CFloat
coordsScale = 32


onePix :: CFloat
onePix = 1 / coordsScale


oneWayPlatformThreshold :: CFloat
oneWayPlatformThreshold = 2 * onePix