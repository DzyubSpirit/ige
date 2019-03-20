module IGE.Graph where

import Protolude
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Map as Map

import IGE.Types

-- Embedded Graph
type EGr n e = Gr (ℂ, n) e
