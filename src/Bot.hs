module Bot
        ( bot
          , graphFromBoard
          , posToIndex
        )
    where

import Vindinium
import Vindinium.Api

import System.Random (getStdRandom, randomR)
import Data.Maybe (fromJust)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Query
import GHC.Exts (sortWith)
import Debug.Trace
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Text (unpack)

bot :: Bot
{-bot = randomBot-}
bot = minerBot
{-bot = eastBot-}

eastBot :: Bot
eastBot _ = return East

randomBot :: Bot
randomBot _ = liftM fromJust $ liftIO $ pickRandom [Stay, North, South, East, West]

attackBot :: Bot
attackBot state = do
    h <- randomHero state
    me <- return $ stateHero state
    return $ getDirection (stateBoard state) (heroPos me) (heroPos h)

minerBot :: Bot
minerBot state = return $ goToMine closestMine
    where closestMine = nearestMine state
          me = stateHero state
          goToMine mine = trace ("Board:\n" ++ printBoard (stateBoard state) ++ "\n" ++ 
                                "Current Position: " ++ show (heroPos me) ++ "\n" ++ 
                                "closestMine: " ++ show closestMine)
            getDirection (stateBoard state) (heroPos me) mine

printBoard :: Board -> String
printBoard b = intercalate "\n" $ chunksOf bsize tiles
  where bsize = boardSize b * 2 --tiles are 2 chars wide
        tiles = unpack $ printTiles $ boardTiles b

nearestMine :: State -> Pos
nearestMine state =
    let board = gameBoard . stateGame $ state
        positions = boardPositions board
        mines = filter (\p -> isEnemyMine state p) positions
        me = stateHero state
        paths = map (shortestPath board (heroPos me)) mines
    in
      {-indexToPos board $ last $ (sortWith length paths) !! 0-}
      case (sortWith length paths) of
        ((n1:n2:ns):_) -> indexToPos board $ last $ (n1:n2:ns)
        {-((n1:n2):_)    -> error "path only 2 long!"-}
        ([]:_)         -> error "path empty!"
        []             -> error $ "no paths at all: length mines = " ++ show (length mines) ++ show (board)

isMine :: Board -> Pos -> Bool
isMine b p = case tileAt b p of
               Just (MineTile _) -> True
               _ -> False

isEnemyMine :: State -> Pos -> Bool
isEnemyMine s p = case tileAt (stateBoard s) p of
               Just (MineTile (Just hID)) 
                | hID == (heroId . stateHero $ s) -> False
                | otherwise                       -> True
               Just (MineTile _) -> True
               _ -> False

boardPositions :: Board -> [Pos]
boardPositions board = positions $ boardSize board
  where positions side = 
          -- x = idx `mod` boardSize
          -- y = idx `div` boardSize
          {-foldl (\ps i -> ps ++ [Pos (i `mod` side) (i `div` side)]) [] [0,1..(side*side)]-}
          foldl (\ps i -> ps ++ [indexToPos board i]) [] [0,1..(side*side)]

getDirection :: Board -> Pos -> Pos -> Dir
getDirection b source dest = 
    if source == dest
      then trace ("Next Step: Stay\n" ++
              "Full path:" ++ show (map (indexToPos b) path) ++ "\n") 
              Stay
      else
        trace ("Next Step: " ++ show dir ++ "\n" ++
              "Full path:" ++ show (map (indexToPos b) path) ++ "\n") 
              dir
        where nextPos = indexToPos b $ path !! 1
              path = shortestPath b source dest
              dir = directionTo source nextPos
              directionTo p1 p2 = 
                case comparePos p1 p2 of
                  [EQ, GT] -> North
                  [EQ, LT] -> South
                  [LT, EQ] -> East
                  [GT, EQ] -> West
                  [_, _]   -> trace "getDirection: invalid move so Stay" Stay  -- Otherwise invalid move so Stay

comparePos :: Pos -> Pos -> [Ordering]
comparePos source dest = 
    let x = compare (posX source) (posX dest)
        y = compare (posY source) (posY dest)
    in [x, y]

graphFromBoard :: Board -> Gr Tile Int
graphFromBoard b = mkGraph lnodes ledges
  where lnodes = map (\i -> (i, (boardTiles b) !! i)) [0,1..n]
        ledges = walkableEdges b
        n = (side * side) - 1
        side = boardSize b

walkableEdges :: Board -> [LEdge Int]
--If i is WoodTile, MineTile, TavernTile, HeroTile then it
--should not have any walkable neighbours because you cant walk onto those
--tiles. (Even though attempting to walk onto a MineTile is how you capture
--it, it doesn't actually move you onto it)
walkableEdges b = concat $ map walkableNeighbours [0,1..n]
  where walkableNeighbours i = map (\t@(p,d) -> (i, posToIndex b p, d)) (validNeighBours i)
        validNeighBours i = case tileAt b (indexToPos b i) of
                              Just FreeTile     -> filter (\ln@(i,_) -> (inBoard b i)) $ neighBours i
                              Just (HeroTile h) -> filter (\ln@(i,_) -> (inBoard b i)) $ neighBours i
                              _ -> []
        side = boardSize b
        n = (side * side) - 1
        neighBours i = [--Positions of neighbours and travel distance of 1
                        (Pos ((i `mod` side) - 1) (i `div` side), 1), --West
                        (Pos ((i `mod` side) + 1) (i `div` side), 1), --East
                        (Pos (i `mod` side) ((i `div` side) - 1), 1), --North
                        (Pos (i `mod` side) ((i `div` side) + 1), 1)] --South

shortestPath :: Board -> Pos -> Pos -> Path
shortestPath b start dest = sp s d g
  where g = graphFromBoard b
        s = (posToIndex b start)
        d = (posToIndex b dest)
   
randomHero :: MonadIO m => State -> m Hero
randomHero state = liftM fromJust $ liftIO $ pickRandom $ gameHeroes (stateGame state)

posToIndex :: Board -> Pos -> Node
posToIndex b p@(Pos x y) = idx
  where idx = y * boardSize b + x

indexToPos :: Board -> Node -> Pos
indexToPos b n = Pos (n `mod` side) (n `div` side) 
  where side = boardSize b

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
