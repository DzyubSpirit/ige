module Main where

import Protolude
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph as Graph
import IGE.Types
import IGE.UI
import IGE.Control

sampleGr :: Gr Weight Weight
sampleGr = mkGraph [(0, 10), (1, 20), (2, 30)] [(0, 1, 1), (1, 2, 2), (0, 2, 3)]

initRM :: RM
initRM = RM (100 :+ 0) (100 :+ 100)

-- (Graph.empty :: Gr () ())
main :: IO ()
main = runMainWindow sampleGr initRM basicKeyBinding
