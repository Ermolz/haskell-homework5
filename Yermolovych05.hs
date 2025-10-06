{-# OPTIONS_GHC -Wall #-}
module Yermolovych05 where

import Data.List (nub)

type GraphS = (Int,[(Int,Int)])            
type Graph  = [[Int]]

-- Задача 1 ------------------------------------
isOrdinary :: Graph -> Bool 
isOrdinary g =
  let n = length g
  in all (\i ->
        all (\v -> v >= 0 && v < n && v /= i && (i `elem` (g !! v))) (g !! i)
        && length (nub (g !! i)) == length (g !! i)
     ) [0..n-1]

-- Задача 2 ------------------------------------
shortWay :: Graph -> Int -> Int -> Maybe [Int] 
shortWay g a b = bfs [[a]] []
  where
    bfs [] _ = Nothing
    bfs (p:ps) v
      | last p == b = Just p
      | last p `elem` v = bfs ps v
      | otherwise = bfs (ps ++ [p ++ [x] | x <- g !! last p, x `notElem` p]) (last p:v)

-- Задача 3 ------------------------------------
isConnecting :: Graph -> Bool 
isConnecting g = length (reachable g 0) == length g
  where
    reachable gr s = dfs [s] []
      where
        dfs [] v = v
        dfs (x:xs) v
          | x `elem` v = dfs xs v
          | otherwise = dfs (xs ++ gr !! x) (x:v)

-- Задача 4 ------------------------------------
components :: Graph -> [[Int]] 
components g = go [0..length g - 1] []
  where
    go [] acc = reverse acc
    go (x:xs) acc
      | any (x `elem`) acc = go xs acc
      | otherwise = let c = reachable g [x] [] in go (xs \\ c) (c:acc)

    reachable _ [] v = v
    reachable gr (x:xs) v
      | x `elem` v = reachable gr xs v
      | otherwise = reachable gr (xs ++ gr !! x) (x:v)

    (\\) a b = filter (`notElem` b) a

-- Задача 5 ------------------------------------
eccentricity :: Graph -> Int -> Int 
eccentricity g v = maximum (bfs [v] [v] 0)
  where
    bfs [] _ d = [d - 1]
    bfs xs visited d =
      let ns = nub (concatMap (g !!) xs) \\ visited
      in if null ns then [d] else bfs ns (visited ++ ns) (d + 1)

    (\\) a b = filter (`notElem` b) a

-- Задача 6 ------------------------------------
findDiameter :: Graph -> Int 
findDiameter g = maximum [eccentricity g v | v <- [0..length g - 1]]

findRadius :: Graph -> Int 
findRadius g = minimum [eccentricity g v | v <- [0..length g - 1]]

-- Задача 7 ------------------------------------
findCenter :: Graph -> [Int] 
findCenter g = [v | v <- [0..length g - 1], eccentricity g v == findRadius g]

-- Задача 8 ------------------------------------
shortWayAll :: Graph -> Int -> Int -> [[Int]] 
shortWayAll g a b = filter ((== minLen) . length) paths
  where
    paths = bfs [[a]] []
    minLen = if null paths then 0 else minimum (map length paths)
    bfs [] _ = []
    bfs (p:ps) v
      | last p == b = p : bfs ps v
      | last p `elem` v = bfs ps v
      | otherwise = bfs (ps ++ [p ++ [x] | x <- g !! last p, x `notElem` p]) (last p:v)

---------------------Тестові дані - Графи -------
gr1S, gr2S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (8,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])

gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]