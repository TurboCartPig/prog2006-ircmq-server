{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ConstraintKinds #-}
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

-- | sendPub - 
sendPub :: Sender t => String -> [String] -> [String] -> String -> Socket z t -> ZMQ z ()
sendPub channel members channels broadcast publisher = do
  send publisher [SendMore] (B.pack channel)
  send publisher [] (C.toStrict . encode $ ResponseMembers {members})
  send publisher [SendMore] (B.pack broadcast)
  send publisher [] (C.toStrict . encode $ ResponseChannels {channels})

-- | mHello - 
-- Hello is the first message from the client,
-- so add them to channel participants and acknowledge the message.
mHello :: (Sender t1, Sender t2) => String -> String -> Socket z t1 -> Socket z t2 -> ChannelParticipants -> ZMQ z ()
mHello name channel responder publisher channels = do
  send responder [] "ACK"
  liftIO $ insertChannelParticipant channels name channel
  members <- liftIO (fetchChannelParticipants channels channel)
  channels <- liftIO (fetchAllChannelNames channels)
  sendPub channel members channels broadcast publisher

-- | mGoodbye -
-- Goodbye is the final message from the client,
-- so remove them from the channel participants and acknowledge the message.
mGoodbye :: (Sender t1, Sender t2) => String -> String -> Socket z t1 -> Socket z t2 -> ChannelParticipants -> ZMQ z ()
mGoodbye name channel responder publisher channels = do
  send responder [] "ACK"
  liftIO $ removeChannelParticipant channels name channel
  members <- liftIO (fetchChannelParticipants channels channel)
  channels <- liftIO (fetchAllChannelNames channels)
  sendPub channel members channels broadcast publisher

-- | mMessage -
-- Acknowledge receiving the message,
-- and pass it on to all clients.
mMessage :: (Sender t1, Sender t2) => [Char] -> String -> [Char] -> Socket z t1 -> Socket z t2 -> ZMQ z ()
mMessage name channel content responder publisher = do
  send responder [] "ACK"
  liftIO (putStrLn $ name ++ ": " ++ content)
  -- Publish the message for all clients to see, on the topic/channel `channel`
  send publisher [SendMore] (B.pack channel)
  send publisher [] (C.toStrict . encode $ Message { name, channel, content })

-- | mReqMembers -
-- Give the client a list of all channel participants.
mReqMembers :: Sender t => String -> ChannelParticipants -> Socket z t -> ZMQ z ()
mReqMembers channel channels responder = do
  members <- liftIO (fetchChannelParticipants channels channel)
  send responder [] (C.toStrict . encode $ ResponseMembers { members })

handleMessageType :: (Sender t, Sender t2) => B.ByteString -> Socket z t -> Socket z t2 -> ChannelParticipants -> ZMQ z ()
handleMessageType buffer responder publisher channels = do
  -- Deserialize the message into a MessageType
        case (decodeStrict buffer :: Maybe MessageType) of
          Just (Hello   name channel)         -> mHello name channel responder publisher channels
          Just (Goodbye name channel)         -> mGoodbye name channel responder publisher channels
          Just (Message name channel content) -> mMessage name channel content responder publisher
          Just (RequestMembers channel)       -> mReqMembers channel channels responder
          _                                   -> do
                                                send responder [] "WHAT"
                                                liftIO (putStrLn "NOT A MESSAGE")

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
        handleMessageType buffer responder publisher channels

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

    -- WTF?
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
  m <- newMVar (Map.empty)
  return (ChannelParticipants m)
