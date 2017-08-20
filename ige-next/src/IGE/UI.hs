module IGE.UI where

import Protolude hiding (on)
import Graphics.UI.Gtk hiding (get)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan

import IGE.Types
import IGE.Drawing
import IGE.Control

runMainWindow :: Gr () () -> RM -> KeyBinding () -> IO ()
runMainWindow initGr initRM keybinding = do
  void initGUI
  w <- windowNew
  da <- drawingAreaNew
  w `containerAdd` da
  editorState <- newTVarIO $ EditorState initGr initRM 3 "" []
  keyChan <- newTBMChanIO 16

  _ <- forkIO $ runKeyBinding keyChan editorState w keybinding

  void $ (w `on` deleteEvent) $ liftIO mainQuit >> return True

  void $ (da `on` exposeEvent) $ liftIO $ do
    dw <- widgetGetDrawWindow da
    es <- readTVarIO editorState
    renderWithDrawable dw $ renderEditorState es
    return True

  void $ (da `on` keyPressEvent) $ do
    kv <- eventKeyVal
    liftIO $ atomically $ writeTBMChan keyChan kv
    return True

  da `widgetSetCanFocus` True
  da `widgetAddEvents` [KeyPressMask]

  widgetShowAll w
  mainGUI