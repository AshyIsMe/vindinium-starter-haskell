module Bot
        (   bot
          , lookupBot
          , minerBot
          , attackBot
          , graphFromState
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
import Data.Map (Map, lookup, fromList)

botMap :: Map String Bot
botMap = fromList [("default", bot),
                   ("attackBot", attackBot),
                   ("minerBot", minerBot)]

lookupBot :: String -> Bot
lookupBot s = case Data.Map.lookup s botMap of
                Just b -> b
                _      -> error "Bot not found!"

bot :: Bot
{-bot = randomBot-}
{-bot = minerBot-}
bot = attackBot

randomBot :: Bot
randomBot _ = liftM fromJust $ liftIO $ pickRandom [Stay, North, South, East, West]

attackBot :: Bot
--If I'm the weakest hero then run to the pub!
--If I'm winning then go enjoy a well earned beer at the pub!
--Attack the hero with the most mines.
  --Ignore bots who are standing on their spawn point. (They might have timed out)
  --Ignore bots who are standing next to a TavernTile.
--If no bots left, then go capture mines instead.
attackBot state = return $ direction
  where 
        heroes = filter validTarget $ reverse $ sortWith heroMineCount $ gameHeroes (stateGame state)
        validTarget = (\h -> not (atHome h) && 
                             not (atTavern (stateBoard state) h))
        runt = weakestHero state
        me = stateHero state
        tavern = nearestTavern state
        mine = nearestEnemyMine state
        neutralMine = nearestNeutralMine state
        direction
          {-| runt == me || (heroLife me) <= 20 =-}
          | (heroLife me) <= 20 =
              trace (printState state ++ "nearestTavern: " ++ show tavern)
              getDirection state (heroPos me) tavern
          | Just p <- neutralMine = 
              trace (printState state ++ "neutralMine: " ++ show neutralMine ++ "\n") 
              getDirection state (heroPos me) p
          | h:_ <- heroes, (heroLife h) <= (heroLife me) =
              case h == me of   --Chill at the pub if I'm winning
                True -> trace (printState state ++ "nearestTavern: " ++ show tavern)
                        getDirection state (heroPos me) tavern
                False -> trace (printState state ++ "most mines Hero: " ++ show h ++ "\n") 
                         getDirection state (heroPos me) (heroPos h)
          | otherwise = 
              trace (printState state ++ "nearestEnemyMine: " ++ show mine ++ "\n") 
              getDirection state (heroPos me) mine

atHome :: Hero -> Bool
atHome h = heroPos h == heroSpawnPos h

atTavern :: Board -> Hero -> Bool
atTavern b h = Just TavernTile `elem` neighbourTiles
  where
    side = boardSize b
    i = posToIndex b $ heroPos h
    neighBours = [Pos ((i `mod` side) - 1) (i `div` side), --West
                  Pos ((i `mod` side) + 1) (i `div` side), --East
                  Pos (i `mod` side) ((i `div` side) - 1), --North
                  Pos (i `mod` side) ((i `div` side) + 1)] --South
    neighbourTiles = map (tileAt b) neighBours

minerBot :: Bot
minerBot state = return $ direction
    where closestMine = nearestEnemyMine state
          me = stateHero state
          tavern = nearestTavern state
          goToMine mine = trace (printState state ++ "closestMine: " ++ show closestMine)
            getDirection state (heroPos me) mine
          direction
            | (heroLife me) <= 20 = goToMine closestMine
            | otherwise = getDirection state (heroPos me) tavern

printState :: State -> String
printState s = "Board:\n" ++ printBoard (stateBoard s) ++ "\n" ++ 
                  "Current Position: " ++ show (heroPos $ stateHero s) ++ "\n" 

printBoard :: Board -> String
printBoard b = intercalate "\n" $ chunksOf bsize tiles
  where bsize = boardSize b * 2 --tiles are 2 chars wide
        tiles = unpack $ printTiles $ boardTiles b

nearestTavern :: State -> Pos
nearestTavern state = nearestTileWith state isTavern

nearestNeutralMine :: State -> Maybe Pos
nearestNeutralMine state = case nearestTileWith state isNeutralMine of
                             p | p /= (heroPos $ stateHero state) -> Just p
                             otherwise -> Nothing

nearestEnemyMine :: State -> Pos
nearestEnemyMine state = nearestTileWith state isEnemyMine

nearestTileWith :: State -> (State -> Pos -> Bool) -> Pos
nearestTileWith state f =
    let board = gameBoard . stateGame $ state
        positions = boardPositions board
        targetTiles = filter (\p -> f state p) positions
        me = stateHero state
        paths = map (shortestPath state (heroPos me)) targetTiles
    in
      case filter ((1<) . length) (sortWith length paths) of
        ((n1:n2:ns):_) -> indexToPos board $ last $ (n1:n2:ns)
        _              -> heroPos me --No targetTiles, just Stay

isMine :: Board -> Pos -> Bool
isMine b p = case tileAt b p of
               Just (MineTile _) -> True
               _ -> False

isTavern :: State -> Pos -> Bool
isTavern s p = case tileAt (stateBoard s) p of
                 Just TavernTile -> True
                 _ -> False

isNeutralMine :: State -> Pos -> Bool
isNeutralMine s p = case tileAt (stateBoard s) p of
                      Just (MineTile Nothing) -> True
                      _                       -> False

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

getDirection :: State -> Pos -> Pos -> Dir
getDirection state source dest = 
    if source == dest
      then trace ("Next Step: Stay\n" ++
              "Full path:" ++ show (map (indexToPos b) path) ++ "\n") 
              Stay
      else
        trace ("Next Step: " ++ show dir ++ "\n" ++
              "Full path:" ++ show (map (indexToPos b) path) ++ "\n") 
              dir
        where nextPos = indexToPos b $ step
              path = shortestPath state source dest
              step = case path of
                       (p1:p2:_) -> p2
                       [p1]      -> p1
                       _         -> posToIndex (stateBoard state) $ heroPos $ stateHero state
              dir = directionTo source nextPos
              b = stateBoard state
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

graphFromState :: State -> Gr Tile Int
graphFromState s = mkGraph lnodes ledges
  where lnodes = map (\i -> (i, (boardTiles b) !! i)) [0,1..n]
        ledges = walkableEdges b (heroId me)
        n = (side * side) - 1
        side = boardSize b
        b = stateBoard s
        me = stateHero s

walkableEdges :: Board -> HeroId -> [LEdge Int]
--If i is WoodTile, MineTile, TavernTile, HeroTile then it
--should not have any walkable neighbours because you cant walk onto those
--tiles. (Even though attempting to walk onto a MineTile is how you capture
--it, it doesn't actually move you onto it). (Our hero's HeroTile is the
--exception that can be pathed through)
walkableEdges b hid = concat $ map walkableNeighbours [0,1..n]
  where walkableNeighbours i = map (\t@(p,d) -> (i, posToIndex b p, d)) (validNeighBours i)
        validNeighBours i = case tileAt b (indexToPos b i) of
                              Just FreeTile     -> filter (\ln@(i,_) -> (inBoard b i)) $ neighBours i
                              Just (HeroTile h) | h == hid -> filter (\ln@(i,_) -> (inBoard b i)) $ neighBours i
                              _ -> []
        side = boardSize b
        n = (side * side) - 1
        neighBours i = [--Positions of neighbours and travel distance of 1
                        (Pos ((i `mod` side) - 1) (i `div` side), 1), --West
                        (Pos ((i `mod` side) + 1) (i `div` side), 1), --East
                        (Pos (i `mod` side) ((i `div` side) - 1), 1), --North
                        (Pos (i `mod` side) ((i `div` side) + 1), 1)] --South

shortestPath :: State -> Pos -> Pos -> Path
shortestPath state start dest = sp s d g
  where g = graphFromState state
        s = (posToIndex b start)
        d = (posToIndex b dest)
        b = stateBoard state

weakestHero :: State -> Hero
weakestHero state = case take 1 (sortWith heroLife $ gameHeroes (stateGame state)) of
                      [h] -> h
   
randomHero :: MonadIO m => State -> m Hero
{-randomHero state = liftM fromJust $ liftIO $ pickRandom $ gameHeroes (stateGame state)-}
randomHero state = liftM fromJust $ liftIO $ pickRandom $ filter (\h -> h /= stateHero state) $ gameHeroes (stateGame state)

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
