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
import Project.Types
import Utils.List (simplifyNames)



-- | Create diagram form Graph
buildDiagram :: DependencyGraph -> Diagram B R2
buildDiagram (PackageGraph pkgs deps) = vertices # applyAll [connectOutside' arrowOpts x y | (x,y) <- deps]  
    where vertices = position (zip (layoutDiagram pkgs deps) (map packageShape pkgs2))
          pkgs2 = zip (simplifyNames pkgs) [1..]


-- Package widget contains package name with some metadata
packageShape :: (String, Int) -> Diagram B R2
packageShape (name, idx) = text name # scale 0.1 # fc black <> roundedRect 0.8 0.6 0.1 # fc yellow # named idx


-- Default arrow options
arrowOpts :: ArrowOpts
arrowOpts = with & headGap  .~ 0.07
                 & tailGap  .~ 0.07
                 & headSize .~ 0.2


-- Create diagram layout
layoutDiagram :: [String] -> [(Int,Int)] -> [P2]
layoutDiagram pkgs deps = map convert $ M.elems (_particles e')
    where e = ensemble pkgs deps 
          e' = forceLayout layoutOptions e
          convert (Particle point _ _) = p2 (unPoint point) 
          
          
layoutOptions :: ForceLayoutOpts (Double, Double)           
layoutOptions = def & damping .~ 0.8 & energyLimit .~ Just 0.001 & stepLimit .~ Nothing

                 
-- Create ensemble to feed force layout algorithm                 
ensemble :: [String] -> [(Int,Int)] -> Ensemble (Double, Double)
ensemble pkgs deps = Ensemble [ (deps,    hookeForce 0.05 4)
                              , (allPairs, coulombForce 0.01)
                              ]
                              particleMap
   where n = length pkgs
         allPairs    = [(x,y) | x <- [1..n], y <- [x+1..n]]
         particleMap = M.fromList . zip [1..n]
                     . map (initParticle . P)
                     $ map (sin &&& cos) [1.0 ..]
                     
