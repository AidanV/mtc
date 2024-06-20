{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import qualified Graphics.Vty as V

import qualified Data.Text as T

import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )

import qualified Brick.Main as M
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , withBorderStyle
  , str
  , txt
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)
import Brick.Types (Widget, BrickEvent(VtyEvent))

data Name = EquationField deriving (Eq, Ord, Show)

data FormInfo = FormInfo { _equation :: T.Text } deriving (Show)

makeLenses ''FormInfo

mkForm :: FormInfo -> Form FormInfo e Name
mkForm = 
    newForm [ editTextField equation EquationField (Just 1) ]
--drawUI :: [T.Widget String]
--drawUI = [ui]
--    where
--        e1 = (E.renderEditor (str . unlines)) "edit 1"
--        -- e2 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit2)
--
--        ui = C.center $
--            (str "Input 1 (unlimited): " <+> (hLimit 30 $ e1 "test")) <=>
--            str " " <=>
--            str "Esc to quit."

draw :: Form FormInfo e Name -> Widget ()
draw f =
    withBorderStyle BS.unicodeRounded $
    B.borderWithLabel (str "label") $
    vLimit 5 $
    C.vCenter $
    renderForm f
    --(str "Name: " <+>) @@=
      --(mkForm (FormInfo { _equation = "" }))

--appEvent :: T.BrickEvent String e -> T.EventM String () ()
--appEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
--    M.halt
----appEvent (T.VtyEvent (V.EvKey (V.KChar '\t') [])) =
----    focusRing %= F.focusNext
----appEvent (T.VtyEvent (V.EvKey V.KBackTab [])) =
----    focusRing %= F.focusPrev
--appEvent ev =   
--    E.handleEditorEvent ev 
--      Just Edit2 -> zoom edit2 $ E.handleEditorEvent ev
--      Nothing -> return ()


--theMap :: A.AttrMap
--theMap = A.attrMap V.defAttr
--    [ (E.editAttr,                   V.white `on` V.blue)
--    , (E.editFocusedAttr,            V.black `on` V.white)
--    ]

--appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
--appCursor = F.focusRingCursor (^.focusRing)

--theApp :: M.App e
--theApp =
--    M.App { M.appDraw = drawUI
--          --, M.appChooseCursor = appCursor
--          , M.appHandleEvent = appEvent
--          , M.appStartEvent = return ()
--          , M.appAttrMap = const theMap
--          }

main :: IO ()
main = 
    let initialState = FormInfo { _equation = "" }
        f = mkForm initialState
        app = M.App { M.appDraw = draw f
                    , M.appHandleEvent = \ev -> do
                      case ev of
                          VtyEvent (V.EvResize {}) -> return ()
                          VtyEvent (V.EvKey V.KEsc []) -> M.halt
                          -- Enter quits only when we aren't in the multi-line editor.
                          _ -> do
                              handleFormEvent ev

                              -- Example of external validation:
                              -- Require age field to contain a value that is at least 18.

                    , M.appStartEvent = return ()
                    }
     in
      M.defaultMain app initialState
    --putStrLn "yuh huh"
--main = do
--    st <- M.defaultMain theApp
--    putStrLn "In input 1 you entered:\n"
--    putStrLn $ unlines $ E.getEditContents "edit 1" 
--    putStrLn "In input 2 you entered:\n"
--    putStrLn $ unlines $ E.getEditContents $ st^.edit2
