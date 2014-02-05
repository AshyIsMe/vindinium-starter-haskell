{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Options.Applicative

import Vindinium
import Bot

import Data.String (fromString)
import Data.Text (pack, unpack)

data Cmd = Training Settings (Maybe Int) (Maybe Board)
         | Arena Settings
         deriving (Show, Eq)

cmdSettings :: Cmd -> Settings
cmdSettings (Training s _ _) = s
cmdSettings (Arena s) = s

settings :: Parser Settings
settings = Settings <$> (Key <$> argument (Just . pack) (metavar "KEY"))
                    <*> (fromString <$> strOption (long "url" <> value "http://vindinium.org"))
                    <*> (fromString <$> strOption (long "bot" <> value "default"))

trainingCmd :: Parser Cmd
trainingCmd = Training <$> settings
                       <*> optional (option (long "turns"))
                       <*> pure Nothing

arenaCmd :: Parser Cmd
arenaCmd = Arena <$> settings

cmd :: Parser Cmd
cmd = subparser
    ( command "training" (info trainingCmd
        ( progDesc "Run bot in training mode" ))
   <> command "arena" (info arenaCmd
        (progDesc "Run bot in arena mode" ))
    )

runCmd :: Cmd -> IO ()
runCmd c  = do
    s <- runVindinium (cmdSettings c) $ do
        case c of
            (Training s t b) -> playTraining t b (lookupBot $ unpack $ settingsBot s)
            (Arena s)              -> playArena (lookupBot $ unpack $ settingsBot s)

    putStrLn $ "Game finished: " ++ unpack (stateViewUrl s)

main :: IO ()
main =
    execParser opts >>= runCmd
  where
    opts = info (cmd <**> helper) idm
