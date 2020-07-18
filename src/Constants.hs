module Constants where


import Foreign.C.Types (CInt, CFloat)
import Linear


minJumpSpeed :: CFloat
minJumpSpeed = 4


playerJumpHeight, playerFallTime :: CFloat
playerJumpHeight = 7
playerFallTime = 0.3


gravity :: CFloat
gravity = 2 * playerJumpHeight / (playerFallTime ** 2)


friction :: CFloat
friction = -60


correctWalkFriction :: CFloat
correctWalkFriction = -300


airFriction :: CFloat
airFriction = -50


coordsScale :: CFloat
coordsScale = 32

onePix :: CFloat
onePix = 1 / coordsScale
