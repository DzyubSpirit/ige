module IGE.UI (
  runMainWindow
) where

import Protolude hiding (on)
import Graphics.UI.Gtk hiding (get)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan

import IGE.Types
import IGE.Render
import IGE.Layout
import IGE.Control

runMainWindow
  :: (NodeType n, EdgeType e) => Gr n e -> RM -> KeyBinding n e () -> IO ()
runMainWindow initGr initRM keybinding = do
  initGUI
  w  <- windowNew
  da <- drawingAreaNew
  w `containerAdd` da
  editorState <- newTVarIO EditorState
    { esGraph   = initGr
    , esRM      = initRM
    , esNum     = noNodes initGr
    , esCmd     = ""
    , esPrompt  = ""
    , esLabels  = []
    , esNodeMap = layoutGr initGr
    }
  keyChan <- newTBMChanIO 16

  _       <- forkIO $ runKeyBinding keyChan editorState w keybinding

  (w `on` deleteEvent) $ liftIO mainQuit >> return True

  (da `on` draw) $ liftIO $ do
    Just dw <- widgetGetWindow da
    es      <- readTVarIO editorState
    dims    <- liftM2 (,) (drawWindowGetWidth dw) (drawWindowGetHeight dw)

    renderWithDrawWindow dw $ dimsRender dims es

  (da `on` keyPressEvent) $ do
    kv <- eventKeyVal
    liftIO $ atomically $ writeTBMChan keyChan kv
    return True

  da `widgetSetCanFocus` True
  da `widgetAddEvents` [KeyPressMask]

  widgetShowAll w
  mainGUI
