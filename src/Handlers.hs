{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers (
  hello,
  goodbye,
  Handlers.message,
  reqMembers,
) where

import           Data
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as C
import           Message
import           System.ZMQ4.Monadic

-- | publish - function to group together some redundant functionality
publish :: (Sender t, ToJSON a) => String -> [String] -> [String] -> String -> Socket z t -> a -> ZMQ z ()
publish channel members channels broadcast publisher msgType = do
  send publisher [SendMore] (B.pack channel)
  send publisher [] (C.toStrict . encode $ ResponseMembers {members})
  send publisher [SendMore] (B.pack broadcast)
  send publisher [] (C.toStrict . encode $ ResponseChannels {channels})
  send publisher [SendMore] (B.pack channel)
  send publisher [] (C.toStrict . encode $ msgType)

-- | hello - first message from the client
-- Acknowledges the message and adds them to channel participants
hello :: (Sender t1, Sender t2) => String -> String -> Socket z t1 -> Socket z t2 -> ChannelParticipants -> ZMQ z ()
hello name channel responder publisher channels = do
  send responder [] "ACK"
  liftIO $ insertChannelParticipant channels name channel
  members <- liftIO (fetchChannelParticipants channels channel)
  channels <- liftIO (fetchAllChannelNames channels)
  publish channel members channels broadcast publisher $ Hello { name, channel }

-- | hello - final message from the client.
-- Acknowledges the message and removes them from the channel participants.
goodbye :: (Sender t1, Sender t2) => String -> String -> Socket z t1 -> Socket z t2 -> ChannelParticipants -> ZMQ z ()
goodbye name channel responder publisher channels = do
  send responder [] "ACK"
  liftIO $ removeChannelParticipant channels name channel
  members <- liftIO (fetchChannelParticipants channels channel)
  channels <- liftIO (fetchAllChannelNames channels)
  publish channel members channels broadcast publisher $ Goodbye { name, channel }

-- | message - Acknowledges receiving the message and passes it to all clients
message :: (Sender t1, Sender t2) => [Char] -> String -> [Char] -> Socket z t1 -> Socket z t2 -> ZMQ z ()
message name channel content responder publisher = do
  send responder [] "ACK"
  liftIO (putStrLn $ name ++ ": " ++ content)
  -- Publish the message for all clients to see, on the topic/channel `channel`
  send publisher [SendMore] (B.pack channel)
  send publisher [] (C.toStrict . encode $ Message { name, channel, content })

-- | reqMembers - gives the client a list of all channel participants
reqMembers :: Sender t => String -> ChannelParticipants -> Socket z t -> ZMQ z ()
reqMembers channel channels responder = do
  members <- liftIO (fetchChannelParticipants channels channel)
  send responder [] (C.toStrict . encode $ ResponseMembers { members })
