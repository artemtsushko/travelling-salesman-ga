-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Artem Tsushko, 2015
-- License     :  BSD3
--
-- Maintainer  :  artem.tsushko@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import TSP.GeneticAlgorithm (solveIO)

main = do
    let cities = 15
        populationSize = 1000
        childrenNumber = 500
        mutantsNumber = 250
        iterations = 100
        maxDistance = 1000
    solveIO cities populationSize childrenNumber mutantsNumber iterations maxDistance
