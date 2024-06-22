{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)

import qualified Debug.Trace

import qualified Data.Text.Zipper

--import Control.Monad (void)
import Lens.Micro ((^.), (^?), over, ix, singular, _head, each)
import Lens.Micro.Extras
import Lens.Micro.TH

import Backend

data Name = EditEquation deriving (Ord, Show, Eq)

data St =
    St { _focusRing :: F.FocusRing Name
       , _editEquation :: E.Editor String Name
       }

makeLenses ''St

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
    where
        e1 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.editEquation)
        ui = C.center $
            (str "Enter Equation: " <+> hLimit 30 e1) <=>
            str " " <=>
            str "Press Tab to switch between editors, Esc to quit."

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
    M.halt
appEvent (T.VtyEvent (V.EvKey (V.KChar '\t') [])) =
    focusRing %= F.focusNext
appEvent (T.VtyEvent (V.EvKey V.KBackTab [])) =
    focusRing %= F.focusPrev
appEvent (T.VtyEvent (V.EvKey V.KEnter [])) =
    --Debug.Trace.trace (calculate "1 + 2") 
    let 
        newState = St (F.focusRing [EditEquation])
                      (E.editor EditEquation (Just 1) (calculate "1 + 2"))
    in
        T.put newState
appEvent ev = do
    r <- use focusRing
    case F.focusGetCurrent r of
      Just EditEquation -> zoom editEquation $ E.handleEditorEvent ev
      Nothing -> return ()

initialState :: St
initialState =
    St (F.focusRing [EditEquation])
       (E.editor EditEquation (Just 1) "")

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr, V.black `on` V.white) ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

theApp :: M.App St e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do
    st <- M.defaultMain theApp initialState
    putStrLn "In input 1 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st^.editEquation