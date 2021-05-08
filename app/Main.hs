{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as C
import qualified Data.Map              as Map
import           Message
import           System.ZMQ4.Monadic

newtype ChannelParticipants = ChannelParticipants (MVar (Map.Map String [String]))

main :: IO ()
main = runZMQ $ do
    -- Socket to talk to individual chat clients
    responder <- socket Rep
    bind responder "tcp://*:5555"

    -- Socket to publish new messages to all chat clients
    publisher <- socket Pub
    bind publisher "tcp://*:6666"

    channels <- liftIO newChannelMap

    forever $ do
        -- Get new message from a client
        buffer <- receive responder

        -- Deserialize the message into a MessageType
        let message = decodeStrict buffer :: Maybe MessageType

        case message of
          -- Hello is the first message from the client,
          -- so add them to channel participants and acknowledge the message.
          Just (Hello   name channel) -> do
            send responder [] "ACK"
            liftIO $ insertChannelParticipant channels name channel
            members <- liftIO (fetchChannelParticipants channels channel)
            send publisher [SendMore] (B.pack channel)
            send publisher [] (C.toStrict . encode $ ResponseMembers {members})

          -- Goodbye is the final message from the client,
          -- so remove them from the channel participants and acknowledge the message.
          Just (Goodbye name channel) -> do
            send responder [] "ACK"
            liftIO $ removeChannelParticipant channels name channel
            members <- liftIO (fetchChannelParticipants channels channel)
            send publisher [SendMore] (B.pack channel)
            send publisher [] (C.toStrict . encode $ ResponseMembers {members})

          -- Acknowledge receiving the message,
          -- and pass it on to all clients.
          Just (Message name channel content) -> do
            send responder [] "ACK"
            liftIO (putStrLn $ name ++ ": " ++ content)

            -- Publish the message for all clients to see, on the topic/channel `channel`
            send publisher [SendMore] (B.pack channel)
            send publisher [] (C.toStrict . encode $ Message { name, channel, content })

          -- Give the client a list of all channel participants.
          Just (RequestMembers channel) -> do
            members <- liftIO (fetchChannelParticipants channels channel)
            send responder [] (C.toStrict . encode $ ResponseMembers { members })

          _ -> do
            send responder [] "WHAT"
            liftIO (putStrLn "NOT A MESSAGE")

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

    -- WTF?
    Nothing -> do
      putMVar cp channels
      putStrLn $ "Tried to remove participant from non-existent channel: " ++ channel

fetchChannelParticipants :: ChannelParticipants -> String -> IO [String]
fetchChannelParticipants (ChannelParticipants cp) channel = do
  channels <- takeMVar cp
  putMVar cp channels
  let res = Map.lookup channel channels
  case res of
    Just chn -> return chn
    Nothing  -> return []

newChannelMap :: IO ChannelParticipants
newChannelMap = do
  m <- newMVar (Map.empty)
  return (ChannelParticipants m)
