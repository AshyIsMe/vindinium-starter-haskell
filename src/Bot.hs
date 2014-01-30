module Bot
        ( bot
        )
    where

import Vindinium

import System.Random (getStdRandom, randomR)
import Data.Maybe (fromJust)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)

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

nearestMine state =
    let board = gameBoard . stateGame $ state
        positions = boardPositions board
        mines = filter (\p -> isMine board p) positions
    in
      -- AA TODO: do distance ranking and choose the actual closest one
      mines !! 0

isMine b p = case tileAt b p of
               Just (MineTile _) -> True
               _ -> False

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

randomHero :: MonadIO m => State -> m Hero
randomHero state = liftM fromJust $ liftIO $ pickRandom $ gameHeroes (stateGame state)

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
