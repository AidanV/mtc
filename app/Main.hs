{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (on)
import Brick.Widgets.Border
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( hLimit,
    str,
    (<+>),
    (<=>),
  )
import qualified Brick.Widgets.Core
import qualified Brick.Widgets.Edit as E
import Calc
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

data Name = EditEquation deriving (Ord, Show, Eq)

data St = St
  { _focusRing :: F.FocusRing Name,
    _previousAnswers :: [String],
    _editEquation :: E.Editor String Name
  }

makeLenses ''St

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
  where
    e1 = F.withFocusRing (st ^. focusRing) (E.renderEditor (str . unlines)) (st ^. editEquation)
    ui =
      C.center $
        Brick.Widgets.Border.borderWithLabel
          (str "Answers")
          ( Brick.Widgets.Core.padLeftRight
              20
              ( Brick.Widgets.Core.padAll
                  1
                  (Brick.Widgets.Core.vBox $ zipWith (\i e -> str ([i] ++ ":  " ++ e)) ['a' ..] (st ^. previousAnswers))
              )
          )
          <=> Brick.Widgets.Border.border (str "= " <+> hLimit 30 e1)

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
  M.halt
appEvent (T.VtyEvent (V.EvKey (V.KChar '\t') [])) =
  focusRing %= F.focusNext
appEvent (T.VtyEvent (V.EvKey V.KBackTab [])) =
  focusRing %= F.focusPrev
appEvent (T.VtyEvent (V.EvKey V.KEnter [])) = do
  s <- T.get
  let ans = show $ calculate $ unlines $ E.getEditContents $ s ^. editEquation
  T.put $
    St
      (F.focusRing [EditEquation])
      (take 10 (ans : (s ^. previousAnswers)))
      (E.editor EditEquation (Just 1) ans)
appEvent ev = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just EditEquation -> zoom editEquation $ E.handleEditorEvent ev
    _ -> return ()

initialState :: St
initialState =
  St
    (F.focusRing [EditEquation])
    []
    (E.editor EditEquation (Just 1) "")

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [(E.editAttr, V.black `on` V.white)]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^. focusRing)

theApp :: M.App St e Name
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = appCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

main :: IO ()
main = do
  st <- M.defaultMain theApp initialState
  putStrLn "In input 1 you entered:\n"
  putStrLn $ unlines $ E.getEditContents $ st ^. editEquation
