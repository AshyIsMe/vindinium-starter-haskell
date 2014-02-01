module GraphTest
  where

import Vindinium
import Vindinium.Api
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Query

board = parseBoard 18 "##############        ############################        ##############################    ##############################$4    $4############################  @4    ########################  @1##    ##    ####################  []        []  ##################        ####        ####################  $4####$4  ########################  $4####$4  ####################        ####        ##################  []        []  ####################  @2##    ##@3  ########################        ############################$-    $-##############################    ##############################        ############################        ##############"

graph = graphFromBoard board

start = labNodes graph !! 7
dest = labNodes graph !! 187

--I'd like to find the shortest path from start to dest in graph but sp is
--the wrong type:
path = sp (fst start) (fst dest) graph

graphFromBoard :: Board -> Gr Tile BiDir
graphFromBoard b = mkGraph lnodes ledges
  where lnodes = map (\i -> (i, (boardTiles b) !! i)) [0,1..n]
        ledges = walkableEdges b
        n = (side * side) - 1
        side = boardSize b

walkableEdges :: Board -> [LEdge BiDir]
walkableEdges b = concat $ map walkableNeighbours [0,1..n]
  where walkableNeighbours i = map (\t@(p,d) -> (i, posToIndex b p, d)) (validNeighBours i)
        validNeighBours i = filter (\ln@(i,_) -> (inBoard b i)) $ neighBours i
        side = boardSize b
        n = (side * side) - 1
        neighBours i = [(Pos ((i `mod` side) - 1) (i `div` side), Horizontal), --West
                        (Pos ((i `mod` side) + 1) (i `div` side), Horizontal), --East
                        (Pos (i `mod` side) ((i `div` side) - 1), Vertical), --North
                        (Pos (i `mod` side) ((i `div` side) + 1), Vertical)] --South

posToIndex :: Board -> Pos -> Int
posToIndex b p@(Pos x y) = idx
  where idx = y * boardSize b + x

inBoard :: Board -> Pos -> Bool
inBoard b (Pos x y) =
    let s = boardSize b
    in x >= 0 && x < s && y >= 0 && y < s
