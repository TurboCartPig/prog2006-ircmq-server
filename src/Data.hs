module Data (
  insertChannelParticipant,
  removeChannelParticipant,
  fetchChannelParticipants,
  fetchAllChannelNames,
  newChannelMap,
  ChannelParticipants,
) where

import           Control.Concurrent
import qualified Data.Map           as Map
import           Message

newtype ChannelParticipants = ChannelParticipants (MVar (Map.Map String [String]))

--
-- | insertChannelParticipant -
--
insertChannelParticipant :: ChannelParticipants -> String -> String -> IO ()
insertChannelParticipant (ChannelParticipants cp) name channel = do
  channels <- takeMVar cp
  let res = Map.lookup channel channels
  case res of
    Just chn -> do
      let new_channel = name : chn
      let channels' = Map.insert channel new_channel channels
      putMVar cp channels'
      putStrLn ("New participant added to the old channel: " ++ channel)

    Nothing  -> do
      let channels' = Map.insert channel [name] channels
      putMVar cp channels'
      putStrLn ("New participant added to the new channel: " ++ channel)

-- | removeChannelParticipant -
--
removeChannelParticipant :: ChannelParticipants -> String -> String -> IO ()
removeChannelParticipant (ChannelParticipants cp) name channel = do
  channels <- takeMVar cp
  let res = Map.lookup channel channels
  case res of
    -- Remove from channel
    Just chn -> do
      let new_channel = filter (/= name) chn
      let channels' = Map.insert channel new_channel channels
      putMVar cp channels'
      putStrLn $ "Removing participant from channel: " ++ channel

    Nothing -> do
      putMVar cp channels
      putStrLn $ "Tried to remove participant from non-existent channel: " ++ channel

-- | fetchChannelParticipants -
--
fetchChannelParticipants :: ChannelParticipants -> String -> IO [String]
fetchChannelParticipants (ChannelParticipants cp) channel = do
  channels <- takeMVar cp
  putMVar cp channels
  let res = Map.lookup channel channels
  case res of
    Just chn -> return chn
    Nothing  -> return []

-- | fetchAllChannelNames -
--
fetchAllChannelNames :: ChannelParticipants -> IO [String]
fetchAllChannelNames (ChannelParticipants cp) = do
  channels <- takeMVar cp
  putMVar cp channels
  return $ Map.keys channels

-- | newChannelMap -
--
newChannelMap :: IO ChannelParticipants
newChannelMap = do
  m <- newMVar Map.empty
  return (ChannelParticipants m)
