module Backend where

import qualified Brick.Main as M
import qualified Brick.Types as T

test :: T.EventM n s ()
test = 
    M.halt