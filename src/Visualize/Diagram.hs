{-# LANGUAGE NoMonomorphismRestriction #-}
{- |
Module : Report.Diagram
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Create diagrams from graph data 
-}
module Visualize.Diagram ( buildDiagram ) where

import Control.Arrow
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Physics.ForceLayout
import qualified Data.Map as M
import Data.AffineSpace.Point
import Data.Default (def)
import Utils.Graph


-- | Create diagram form Graph
buildDiagram :: NamesGraph -> Diagram B R2
buildDiagram (Graph vs es) = vertices # applyAll [connectOutside' arrowOpts x y | (x,y) <- es]  
    where vertices = position (zip (layoutDiagram (Graph vs es)) (map packageShape vs))


-- Package widget contains package name with some metadata
packageShape :: String -> Diagram B R2
packageShape name = text name # scale 0.1 # fc black <> roundedRect 0.8 0.6 0.1 # fc yellow # named name


-- Default arrow options
arrowOpts :: ArrowOpts
arrowOpts = with & headGap  .~ 0.07
                 & tailGap  .~ 0.07
                 & headSize .~ 0.2


-- Create diagram layout
layoutDiagram :: NamesGraph -> [P2]
layoutDiagram graph = map convert $ M.elems (_particles e')
    where e = ensemble graph 
          e' = forceLayout layoutOptions e
          convert (Particle point _ _) = p2 (unPoint point) 
          
          
layoutOptions :: ForceLayoutOpts (Double, Double)           
layoutOptions = def & damping .~ 0.8 & energyLimit .~ Just 0.001 & stepLimit .~ Nothing

                 
-- Create ensemble to feed force layout algorithm                 
ensemble :: NamesGraph -> Ensemble (Double, Double)
ensemble (Graph vs es) = Ensemble [ (edges,    hookeForce 0.05 4)
                                 , (allPairs, coulombForce 0.01)
                                 ]
                                 particleMap
   where n = length vs
         edges       = edgeIds (Graph vs es)
         allPairs    = [(x,y) | x <- [1..n], y <- [x+1..n]]
         particleMap = M.fromList . zip [1..n]
                     . map (initParticle . P)
                     $ map (sin &&& cos) [1.0 ..]
                     
-- Convert edges with names into edges with Ids
edgeIds :: NamesGraph -> [(Int, Int)]
edgeIds (Graph vs es) = map ((M.!) ids *** (M.!) ids) es
    where ids = M.fromList (zip vs [1..]) 
                     
