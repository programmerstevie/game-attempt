module Controls where

import ECS.Base


noControls :: ControlInput
noControls = ControlInput False False False False

rightControl :: ControlInput
rightControl = noControls{ rightKey = True }

leftControl :: ControlInput
leftControl = noControls{ leftKey = True }

turnAround :: ControlInput -> ControlInput
turnAround c = c { rightKey = leftKey c, leftKey = rightKey c }
