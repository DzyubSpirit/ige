module IGE.Control
  ( runKeyBinding
  , basicKeyBinding
  , defaultKeyBinding
  , showLabels
  )
  where

import Prelude (String)
import Protolude hiding (empty, readFile, writeFile)
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform
import Conduit
import Data.Conduit.TMChan
import Data.Conduit.Combinators (peek)
import Data.Text.Read (decimal)
import Graphics.UI.Gtk hiding (get, Weight)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Control.Concurrent.STM
import qualified Data.Sequence as Seq
import Data.Sequence ((<|), (|>), viewl, ViewL(..), Seq(..))
import Control.Monad.Trans.Maybe
import Data.Text (pack)
import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString.Lazy (readFile, writeFile)
import System.Directory (doesFileExist)

import IGE.Types
import IGE.Keys
import IGE.Layout
import IGE.Serialization
import IGE.Render

instance Inputable Node where
  readInput = MaybeT (showLabels >> loop)
    where
      clear = clearLabels >> return Nothing
      loop = (readCharKey >>=) $ maybe clear $ \ch ->
        matchingLabels ch >>= \x -> case x of
          [] -> clear
          [(_, node)] -> clearLabels >> return (Just node)
          labels -> updateLabels labels >> loop

readCharKey :: KeyBinding n e (Maybe Char)
readCharKey = awaitOrFinish Nothing $ return . keyToChar

matchingLabels :: Char -> KeyBinding n e [(String, Node)]
matchingLabels ch = do
  labels <- runTVarReader $ view _labels
  return [ label | label@(x:_, _) <- labels, x == ch ]

updateLabels :: [(String, Node)] -> KeyBinding n e ()
updateLabels labels = updateEditor $ _labels .= labels'
  where labels' = [ (xs, node) | (_:xs, node) <- labels ]

labelChars = "asdfghjkl"

makeLabels :: Int -> String -> [String]
makeLabels n alphabet = helper n alphabet "" Seq.empty
 where
  helper :: Int -> String -> String -> Seq String -> [String]
  helper 0 _ _ queue = reverse <$> toList queue
  helper n (c:cs) postfix queue =
    helper (n - 1) cs postfix (queue |> (c : postfix))
  helper n [] _ queue = case viewl queue of
    a:<rest -> helper (n + 1) alphabet a rest
    EmptyL  -> []

showLabels :: KeyBinding n e ()
showLabels = updateEditor $ do
  gr <- use _graph
  _labels .= labelGraph gr

clearLabels :: KeyBinding n e ()
clearLabels = updateEditor $ _labels .= []

labelGraph :: Gr n e -> [(String, Node)]
labelGraph gr = zip labels (nodes gr)
  where labels = makeLabels (length $ nodes gr) labelChars

instance Inputable String where
  readInput = MaybeT loop
    where
      handleInput c
        | c == '\b' = updateEditor (_cmd %= tailDef []) >> loop
        | c == '\r'= do
          s <- runTVarReader $ view _cmd
          updateEditor (_cmd .= "") >> return (Just $ reverse s)
        | c == '\ESC' = updateEditor (_cmd .= "") >> return Nothing
        | otherwise = updateEditor (_cmd %= (c:)) >> loop

      loop = awaitOrFinish Nothing $ \kv -> do
        case keyToChar kv of
          (Just c) -> handleInput c
          Nothing -> loop


instance Inputable () where
  readInput = return ()
  readInputPrompt _ = readInput

instance Inputable Text where
  readInput = pack <$> readInput

instance Inputable Weight where
  readInput = do
    s <- readInput
    either (\err -> readInputPrompt $ "Incorrect format for integer: "++err++".Try again: ") (return . Weight . fst) $ decimal s

instance NodeType Text where

instance NodeType Weight where

instance EdgeType Text where

instance EdgeType Weight where

instance NodeType () where

instance EdgeType () where

-- to do: refactor these into pure code, and make a new type for "action that could fail"

addNode :: (Inputable n) => MaybeT (KeyBinding n e) ()
addNode = do
  label <- readInputPrompt "label: "
  lift $ updateEditorLayout $ do
    n <- use _num
    _graph %= insNode (n, label)
    _num += 1

linkNodes :: (Inputable e) => MaybeT (KeyBinding n e) ()
linkNodes = do
  n1    <- readInputPrompt "SELECT NODE 1"
  n2    <- readInputPrompt "SELECT NODE 2"
  label <- readInputPrompt "label: "
  lift $ updateEditorLayout $ _graph %= insEdge (n1, n2, label)

delete :: MaybeT (KeyBinding n e) ()
delete = do
  lift showLabels
  kv <- lift await
  let edgeOrNode kv = case keyToChar kv of
        Nothing -> MaybeT $ Just <$> clearLabels
        Just ch ->
          if ch == 'e' then deleteEdge else lift (leftover kv) >> deleteNode
  maybe (return ()) edgeOrNode kv

deleteNode :: MaybeT (KeyBinding n e) ()
deleteNode = do
  node <- readInput
  lift $ updateEditor $ _graph %= delNode node

deleteEdge :: MaybeT (KeyBinding n e) ()
deleteEdge = do
  node1 <- readInputPrompt "SELECT NODE 1"
  node2 <- readInputPrompt "SELECT NODE 2"
  lift $ updateEditor $ _graph %= delEdge (node1, node2)

readGraph :: (FromJSON n, FromJSON e) => MaybeT (KeyBinding n e) ()
readGraph = do
  filename <- readInputPrompt "filename: "
  liftIO (doesFileExist filename) >>= guard
  newgraph <- MaybeT (graphFromBS <$> liftIO (readFile filename))
  lift $ updateEditorLayout $ _graph .= newgraph

writeGraph :: (ToJSON n, ToJSON e) => MaybeT (KeyBinding n e) ()
writeGraph = do
  fn <- readInputPrompt "filename: "
  g  <- lift $ runTVarReader $ view _graph
  liftIO $ writeFile fn $ graphToBS g

transRel :: ℂ -> EditorM n e ()
transRel z = do
  r <- use $ _rm . _r
  _rm . _trans += (r :+ 0) * z

navigationKeyMap :: Map.Map KeyVal (EditorM n e ())
navigationKeyMap = Map.fromList
  [ (xK_plus   , _rm . _at *= ((10 / 9) :+ 0))
  , (xK_minus  , _rm . _at *= ((9 / 10) :+ 0))
  , (xK_h      , transRel (0.5 :+ 0))
  , (xK_j      , transRel (0 :+ (-0.5)))
  , (xK_k      , transRel (0 :+ 0.5))
  , (xK_l      , transRel ((-0.5) :+ 0))
  , (xK_greater, _rm . _at *= cis (pi / 6))
  , (xK_less   , _rm . _at *= cis ((-pi) / 6))
  ]

basicKeyBinding :: (NodeType n, EdgeType e) => KeyBinding n e ()
basicKeyBinding = loop
 where
  loop = awaitOrFinish () $ \kv -> case Map.lookup kv navigationKeyMap of
    (Just em) -> do
      updateEditor em
      loop
    Nothing -> handleSpecial kv
  handleSpecial kv
    | kv == xK_colon = do
      runMaybeT $ do
        cmd <- readInputPrompt ":"
        putText cmd
      loop
    | kv == xK_a = runMaybeT addNode >> loop
    | kv == xK_c = runMaybeT linkNodes >> loop
    | kv == xK_d = runMaybeT delete >> loop
    | kv == xK_w = runMaybeT writeGraph >> loop
    | kv == xK_o = runMaybeT readGraph >> loop
    | kv == xK_q = liftIO mainQuit >> return ()
    | otherwise = loop

defaultKeyBinding :: (NodeType n, EdgeType e) => KeyVal -> KeyBinding n e ()
defaultKeyBinding kv = case Map.lookup kv navigationKeyMap of
  (Just em) -> updateEditor em
  Nothing   -> void $ handleSpecial kv
   where
     {-
    handleSpecial
      :: (ToJSON n, FromJSON n, Inputable n, ToJSON e, FromJSON e, Inputable e)
      => KeyVal
      -> KeyBinding n e (Maybe ())
-}
    handleSpecial kv
      | kv == xK_colon = runMaybeT $ do
        cmd <- readInputPrompt ":"
        putText cmd
      | kv == xK_a = runMaybeT addNode
      | kv == xK_c = runMaybeT linkNodes
      | kv == xK_d = runMaybeT delete
      | kv == xK_w = runMaybeT writeGraph
      | kv == xK_o = runMaybeT readGraph
      | kv == xK_q = liftIO mainQuit >> return (Just ())
      | otherwise = return $ Just ()

refresh :: (WidgetClass w) => w -> Consumer RefreshType (IGEM n e) ()
refresh widget = do
  rt <- await
  if rt == Just LayoutChange
    then runTVarState $ do
      graph <- use _graph
      _nodeMap .= layoutGr graph
    else return ()
  liftIO $ widgetQueueDraw widget
  refresh widget

runKeyBinding
  :: (WidgetClass w, NodeType n, EdgeType e)
  => TBMChan KeyVal
  -> TVar (EditorState n e)
  -> w
  -> KeyBinding n e ()
  -> IO ()
runKeyBinding keyChan editorState w kb =
  runIGEM editorState $ runConduit $ sourceTBMChan keyChan .| kb .| refresh w
