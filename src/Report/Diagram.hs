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
module Report.Diagram ( buildDiagram ) where

import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Utils.Graph


-- | Create diagram form Graph
buildDiagram :: NamesGraph -> Diagram B R2
buildDiagram (Graph vs es) = vertices # applyAll [connectOutside' arrowOpts x y | (x,y) <- es]  
    where n = length vs 
          vertices = decorateTrail (regPoly n 1) (map packageNode vs)


-- Package widget
packageNode :: String -> Diagram B R2
packageNode name = text name # scale 0.1 # fc black <> circle 0.2 # fc yellow # named name


-- Default arrow options
arrowOpts :: ArrowOpts
arrowOpts = with & headGap  .~ 0.07
                 & tailGap  .~ 0.07
                 & headSize .~ 0.2
