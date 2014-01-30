module Bot
        ( bot
        )
    where

import Vindinium

import System.Random (getStdRandom, randomR)
import Data.Maybe (fromJust)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

bot :: Bot
{-bot = randomBot-}
bot = attackBot

attackBot :: Bot
attackBot state = 
    let h = randomHero state
        me = stateHero state
    in getDirection (heroPos me) (heroPos h)
    {-How do I put the Dir from getDirection into the Vindinium monad?-}

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

randomHero :: State -> IO (Maybe Hero)
randomHero state = pickRandom $ gameHeroes (stateGame state)

randomBot :: Bot
randomBot _ = liftM fromJust $ liftIO $ pickRandom [Stay, North, South, East, West]

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
