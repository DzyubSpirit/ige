module IGE.Render where

import Protolude
import IGE.Types
import IGE.Layout
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Text (pack)
import Graphics.Rendering.Cairo
import Lens.Micro.Platform
import qualified Data.Map.Strict as Map

-- Refactoring ideas:
-- - Use diagrams

instance Renderable Text where
  render s (x :+ y) = do
    extents <- textExtents s
    rectangle
      (x - textMargin)
      (y - (textExtentsHeight extents + textMargin))
      (textExtentsWidth extents + (2 * textMargin))
      (textExtentsHeight extents + (2 * textMargin))
    setSourceRGB 0.1 0.1 0.1
    fill
    setSourceRGB 0.9 0.9 0.9
    moveTo x y
    showText s

instance Renderable () where
  render () (x :+ y) = do
    arc x y nodeSize 0 (2 * pi)
    fill

instance Renderable Weight where
  render n = render (pack $ show n)

instance RenderNode Text where
  renderNode t c@(x :+ y) = do
    let c' = x :+ (y + 20)
    render () c
    render t c'

instance RenderNode Weight where
  renderNode n = renderNode (pack $ show n)

instance RenderEdge Text where
  renderEdge s p1@(x1 :+ y1) p2@(x2 :+ y2) = do
    extents <- textExtents s
    let r = sqrt $ (y2-y1)^2 + (x2-x1)^2
        evx :+ evy = (p2 - p1) / (r :+ 0)
        pVec = (-evy) :+ evx
        pad = textExtentsHeight extents + textExtentsWidth extents
        p = (p1 + p2) / 2 + pVec * (pad :+ 0)
    render s p

instance RenderEdge Weight where
  renderEdge w = renderEdge (pack $ show w)

instance RenderNode () where

instance RenderEdge () where
  renderEdge _ _ _ = return ()

nodeSize :: Double
nodeSize = 4

renderBackground :: Render ()
renderBackground = do
  setSourceRGB 0.1 0.1 0.1
  paint
  setSourceRGB   0.9                   0.9             0.9
  selectFontFace ("monospace" :: Text) FontSlantNormal FontWeightNormal
  setFontSize 15

renderNodes :: (RenderNode a) => [(ℂ, a)] -> Render ()
renderNodes nodes = mapM_ (uncurry $ flip renderNode) nodes

arrowWidth = 30
arrowHeight = 5

renderEdges :: (RenderEdge a) => [(ℂ, ℂ, a)] -> Render ()
renderEdges edges = forM_ edges $ \(p0@(x0:+y0), p1@(x1:+y1), label) -> do
  moveTo x0 y0
  lineTo x1 y1
  let r             = sqrt $ (x1 - x0) ^ 2 + (y1 - y0) ^ 2
      ev@(evx:+evy) = (p1 - p0) / (r :+ 0)
      pv            = (-evy) :+ evx
      x2:+y2        = p1 - ev * arrowWidth + pv * arrowHeight
      x3:+y3        = p1 - ev * arrowWidth - pv * arrowHeight
  moveTo x1 y1
  lineTo x2 y2
  moveTo x1 y1
  lineTo x3 y3
  stroke
  renderEdge label p0 p1

renderCommand :: (Int, Int) -> [Char] -> Render ()
renderCommand (w, h) s = do
  extents <- textExtents s
  moveTo 0 ((fromIntegral h) - (textExtentsHeight extents))
  showText s

textMargin = 3

renderLabels :: [([Char], ℂ)] -> Render ()
renderLabels labels = forM_ labels $ \(s, x:+y) -> do
  extents <- textExtents s
  rectangle (x - textMargin)
            (y - (textExtentsHeight extents + textMargin))
            (textExtentsWidth extents + (2 * textMargin))
            (textExtentsHeight extents + (2 * textMargin))
  setSourceRGB 0.7 0.7 0.2
  fill
  setSourceRGB 0.1 0.1 0.1
  moveTo x y
  showText s

instance (RenderNode n, RenderEdge e) => DimsRenderable (EditorState n e) where
  dimsRender dims es  =do
    let rm      = es ^. _rm
    let graph   = es ^. _graph
    let labels  = es ^. _labels
    let nodeMap = (rm ^*) <$> es ^. _nodeMap
    renderBackground
    renderEdges
      $   ((_1 %~ (nodeMap Map.!)) . (_2 %~ (nodeMap Map.!)))
      <$> labEdges graph
    renderNodes $ (_1 %~ (nodeMap Map.!)) <$> labNodes graph
    renderCommand dims (es ^. _prompt ++ reverse (es ^. _cmd))
    renderLabels $ over _2 (nodeMap Map.!) <$> labels
