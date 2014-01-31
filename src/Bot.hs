module Bot
        ( bot
        )
    where

import Vindinium

import System.Random (getStdRandom, randomR)
import Data.Maybe (fromJust)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Graph

bot :: Bot
{-bot = randomBot-}
bot = minerBot

randomBot :: Bot
randomBot _ = liftM fromJust $ liftIO $ pickRandom [Stay, North, South, East, West]

attackBot :: Bot
attackBot state = do
    h <- randomHero state
    me <- return $ stateHero state
    return $ getDirection (heroPos me) (heroPos h)

minerBot :: Bot
minerBot state = return $ goToMine closestMine
    where closestMine = nearestMine state
          me = stateHero state
          goToMine mine = getDirection (heroPos me) mine

nearestMine :: State -> Pos
nearestMine state =
    let board = gameBoard . stateGame $ state
        positions = boardPositions board
        mines = filter (\p -> isMine board p) positions
        me = stateHero state
        reachableNodes = dfs (graphFromBoard board) [posToIndex board (heroPos me)] !! 0
    in
      -- AA TODO: do distance ranking and choose the actual closest one
      -- reachableNodes is a tree of nodes my hero can reach.
      -- Need a function: shortestPath reachableNodes targetNode
      mines !! 0

isMine :: Board -> Pos -> Bool
isMine b p = case tileAt b p of
               Just (MineTile _) -> True
               _ -> False

boardPositions :: Board -> [Pos]
boardPositions board = positions $ boardSize board
  where positions side = 
          -- x = idx `mod` boardSize
          -- y = idx `div` boardSize
          foldl (\ps i -> ps ++ [Pos (i `mod` side) (i `div` side)]) [] [0,1..(side*side)]

getDirection :: Pos -> Pos -> Dir
getDirection source dest =
    if source == dest
      then Stay
      else
        case comparePos source dest of 
          [GT,_] -> West
          [LT,_] -> East
          [_,GT] -> North
          [_,LT] -> South

comparePos :: Pos -> Pos -> [Ordering]
comparePos source dest = 
    let x = compare (posX source) (posX dest)
        y = compare (posY source) (posY dest)
    in [x, y]

graphFromBoard :: Board -> Graph
graphFromBoard b = buildG (0, side * side) edges
  where edges = walkableEdges b
        side = boardSize b

walkableEdges :: Board -> [Edge]
walkableEdges b = concat $ map walkableNeighbours [0,1..n]
  where walkableNeighbours i = map (\p -> (i, posToIndex b p)) (validNeighBours i)
        validNeighBours i = (filter (inBoard b) $ neighBours i)
        side = boardSize b
        n = (side * side) - 1
        neighBours i = [Pos ((i `mod` side) - 1) (i `div` side), --West
                        Pos ((i `mod` side) + 1) (i `div` side), --East
                        Pos (i `mod` side) ((i `div` side) - 1), --North
                        Pos (i `mod` side) ((i `div` side) + 1)] --South

randomHero :: MonadIO m => State -> m Hero
randomHero state = liftM fromJust $ liftIO $ pickRandom $ gameHeroes (stateGame state)

posToIndex :: Board -> Pos -> Int
posToIndex b p@(Pos x y) = idx
  where idx = y * boardSize b + x

inBoard :: Board -> Pos -> Bool
inBoard b (Pos x y) =
    let s = boardSize b
    in x >= 0 && x < s && y >= 0 && y < s

tileAt :: Board -> Pos -> Maybe Tile
tileAt b p@(Pos x y) =
    if inBoard b p
        then Just $ boardTiles b !! idx
        else Nothing
  where
    idx = y * boardSize b + x

pickRandom :: [a] -> IO (Maybe a)
pickRandom [] = return Nothing
pickRandom xs = do
    idx <- getStdRandom (randomR (0, length xs - 1))
    return . Just $ xs !! idx
