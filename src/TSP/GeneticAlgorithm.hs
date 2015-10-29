-----------------------------------------------------------------------------
--
-- Module      :  TSP.GeneticAlgorithm
-- Copyright   :  (c) Artem Tsushko, 2015
-- License     :  BSD3
--
-- Maintainer  :  artem.tsushko@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- | Solves travelling salesman problem using genetic algorithm
--
-----------------------------------------------------------------------------

module TSP.GeneticAlgorithm (
    City,
    Path,
    Distance,
    inversion,
    uninversion,
    randomDistanceMatrix,
    randomPaths,
    iteration,
    bestResult,
    solveIO
) where

import System.Random (StdGen, getStdGen, newStdGen, randomR)
import Data.Array (listArray, (!), Array)
import Data.List (sortOn, minimumBy)
import Data.Ord (comparing)

type City = Int
type Path = [City]
type Distance = Integer


-- | calculates total distance of path by distance matrix
distance :: Array (City,City) Distance -> Path -> Distance
distance matrix cities@(city:_) = distance' matrix city cities
distance' matrix firstCity [city] = matrix ! (city, firstCity)
distance' matrix firstCity (city:cities) = matrix ! (city, head cities)
                                         + distance' matrix firstCity cities


-- | generates a random distance matrix
randomDistanceMatrix :: Int -> Distance -> StdGen -> Array (Int,Int) Distance
randomDistanceMatrix n maxDistance gen = listArray ((1,1), (n,n)) $ randomDistanceMatrix' gen 1 n
    where randomDistanceMatrix' :: StdGen -> Int -> Int -> [Distance]
          randomDistanceMatrix' gen i n =
             let (nextGen, row) = randomRow gen i n
             in  if i == n
                 then row
                 else row ++ randomDistanceMatrix' nextGen (succ i) n
          randomRow :: StdGen -> Int -> Int -> (StdGen,[Distance])
          randomRow gen col n =
              foldr (\x (gen,list) -> if x == col
                                      then (gen,0:list)
                                      else let (y, nextGen) = randomR (0, maxDistance) gen
                                      in (nextGen,y:list))
                    (gen,[]) [1..n]


{-| For a permutation i1, i2, . . . , iN of the set {1, 2, . . . , N}
    let aj denote the number of integers in the permutation which precede j
    but are greater than j. The sequence of numbers a1, a2, . . . , aN is called
    the inversion sequence of the permutation i1, i2, . . . , iN .
-}
inversion :: [Int] -> [Int]
inversion ints = map (\i -> length . filter (>i) . takeWhile (/= i) $ ints) [1..length ints]


-- | recovers permutation from it's inversion
uninversion :: [Int] -> [Int]
uninversion inv = snd . foldr (\n (i, perm) -> (i-1, advanceIthByN perm i n))
                            (length inv, [1..length inv]) $ inv


-- | advances ith element's position by n in permutation perm
advanceIthByN :: [Int] -> Int -> Int -> [Int]
advanceIthByN perm i n = take (i - 1) perm
                        ++ take n (drop i perm)
                        ++ (perm !! (i - 1)) : drop (i + n) perm


-- | generates random inversion of permutation of given size
randomInversion :: Int -> StdGen ->  ([Int], StdGen)
randomInversion = randomInversionR 1
    where randomInversionR cur size gen
            | cur == size = ([0],gen)
            | otherwise = let
                (elems, oldGen) = randomInversionR (succ cur) size gen
                (elem, newGen) = randomR (0 , size - cur) oldGen
                in (elem:elems , newGen)


-- | generates random path of given size
randomPath :: Int -> StdGen -> ([City], StdGen)
randomPath size gen = let (inv, newGen) = randomInversion size gen
                      in (uninversion inv, newGen)


-- | generates 'count' random paths
randomPaths :: Int -> Int -> StdGen -> ([Path], StdGen)
randomPaths count size gen =
    iterate (\(paths, oldGen) ->
        let (path, newGen) = randomPath size oldGen
        in (path:paths, newGen))
    ([],gen)
    !! count


-- | selects one path from given set of paths
tournament :: (Path -> Distance) -> [Path] -> Int -> StdGen -> (Path, StdGen)
tournament distanceFunc candidates numberCandidates gen =
    let (index1, gen1) = randomR (0, numberCandidates - 1) gen
        (index2, gen2) = randomR (0, numberCandidates - 1) gen1
        candidate1 = candidates !! index1
        candidate2 = candidates !! index2
        winner = if distanceFunc candidate1 < distanceFunc candidate2
                 then candidate1  else candidate2
    in (winner, gen2)


-- | selects 'resultSize' paths from given set of paths by tournament procedure
selection :: (Path -> Distance) -> [Path] -> Int -> Int -> StdGen -> ([Path], StdGen)
selection distanceFunc candidates numberCandidates resultSize gen =
    iterate (\(paths, gen) ->
        let (path, gen2) = tournament distanceFunc candidates numberCandidates gen
        in (path:paths, gen2))
    ([], gen)
    !! resultSize


-- | take two paths and returns these two path after applying crossover operation
crossover :: Path -> Path -> StdGen -> (Path, Path, StdGen)
crossover path1 path2 gen =
    let (firstK, newGen) = randomR (1, length path1 - 1) gen
        inversion1 = inversion path1
        inversion2 = inversion path2
        newInversion1 = take firstK inversion1 ++ drop firstK inversion2
        newInversion2 = take firstK inversion2 ++ drop firstK inversion1
        newPath1 = uninversion newInversion1
        newPath2 = uninversion newInversion2
    in (newPath1, newPath2, newGen)


-- | crosses (by pair) all given paths
crossing :: [Path] -> StdGen -> ([Path], StdGen)
crossing (path1:path2:rest) gen =
    let (newRest, gen1) = crossing rest gen
        (newPath1, newPath2, gen2) = crossover path1 path2 gen1
    in (newPath1 : newPath2 : newRest, gen2)
crossing _ gen = ([], gen)


-- | mutates given path
mutate :: Path -> StdGen -> (Path, StdGen)
mutate path gen =
    let (rand1, gen1) = randomR (1, length path) gen
        (rand2, gen2) = randomR (1, length path) gen1
        oldPos = min rand1 rand2
        newPos = max rand1 rand2
    in (advanceIthByN path oldPos $ newPos - oldPos , gen2)


-- | mutates path with probabillity 'mutantsCount/candidatesCount'
probablyMutate :: Path -> Int -> Int -> StdGen -> (Path, StdGen)
probablyMutate candidate candidatesCount mutantsCount gen =
    let (n, newGen) = randomR (1, candidatesCount) gen
    in if n <= mutantsCount
       then mutate candidate newGen
       else (candidate, newGen)


-- | mutates each of given paths with probabillity 'mutantsCount/candidatesCount'
mutation :: [Path] -> Int -> Int -> StdGen -> ([Path],StdGen)
mutation candidates candidatesCount mutantsCount gen =
    foldr (\candidate (results, gen1) ->
        let (probableMutant, gen2) = probablyMutate
                candidate candidatesCount mutantsCount gen1
        in (probableMutant : results, gen2)) ([],gen) candidates


-- | performs selection, crossing and mutation one time
iteration :: (Path -> Distance) -> [Path] -> Int -> Int -> Int -> StdGen-> ([Path],StdGen)
iteration distanceFunc paths pathsNumber childrenNumber mutantsNumber gen = let
    (selected, gen1) = selection distanceFunc paths pathsNumber childrenNumber gen
    (crossed, gen2) = crossing selected gen1
    sorted = sortOn distanceFunc paths
    mixed = take (pathsNumber - childrenNumber) sorted ++ crossed
    in mutation mixed pathsNumber mutantsNumber gen


-- | selects path with minimal value of distance function from given set of paths
bestResult :: (Path -> Distance) -> [Path] -> (Path, Distance)
bestResult distanceFunc paths = let
    path = minimumBy (comparing distanceFunc) paths
    dist = distanceFunc path
    in (path,dist)


-- | solves the problem printing intermediate result to stdout
solveIO :: Int -> Int-> Int -> Int -> Int -> Distance -> IO ()
solveIO cities populationSize childrenNumber mutantsNumber iterations maxDistance = do
    gen <- getStdGen
    -- generate distances
    let distanceFunc = distance $ randomDistanceMatrix cities maxDistance gen
    gen1 <- newStdGen
    -- generate first population
    let (paths, _) = randomPaths populationSize cities gen1
    let printNextIteration 0 paths = return ()
        printNextIteration i paths = do
            gen <- newStdGen
            let (betterPaths, _) = iteration
                    distanceFunc paths populationSize
                    childrenNumber mutantsNumber gen
                (path,dist) = bestResult distanceFunc betterPaths
            putStrLn $ show (iterations - i) ++ ". "
                       ++ show path ++ " (" ++ show dist ++ ")"
            printNextIteration (i-1) betterPaths
    -- iterate
    printNextIteration iterations paths
