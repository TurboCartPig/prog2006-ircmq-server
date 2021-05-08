{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers
    ( hello
    , goodbye
    , Handlers.message
    , reqMembers
    ) where

import Message
import Data
import           System.ZMQ4.Monadic
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as C

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
hello :: (Sender t1, Sender t2) => String -> String -> Socket z t1 -> Socket z t2 -> ChannelParticipants -> ZMQ z ()
hello name channel responder publisher channels = do
  send responder [] "ACK"
  liftIO $ insertChannelParticipant channels name channel
  members <- liftIO (fetchChannelParticipants channels channel)
  channels <- liftIO (fetchAllChannelNames channels)
  sendPub channel members channels broadcast publisher

-- | mGoodbye -
-- Goodbye is the final message from the client,
-- so remove them from the channel participants and acknowledge the message.
goodbye :: (Sender t1, Sender t2) => String -> String -> Socket z t1 -> Socket z t2 -> ChannelParticipants -> ZMQ z ()
goodbye name channel responder publisher channels = do
  send responder [] "ACK"
  liftIO $ removeChannelParticipant channels name channel
  members <- liftIO (fetchChannelParticipants channels channel)
  channels <- liftIO (fetchAllChannelNames channels)
  sendPub channel members channels broadcast publisher

-- | mMessage -
-- Acknowledge receiving the message,
-- and pass it on to all clients.
message :: (Sender t1, Sender t2) => [Char] -> String -> [Char] -> Socket z t1 -> Socket z t2 -> ZMQ z ()
message name channel content responder publisher = do
  send responder [] "ACK"
  liftIO (putStrLn $ name ++ ": " ++ content)
  -- Publish the message for all clients to see, on the topic/channel `channel`
  send publisher [SendMore] (B.pack channel)
  send publisher [] (C.toStrict . encode $ Message { name, channel, content })

-- | mReqMembers -
-- Give the client a list of all channel participants.
reqMembers :: Sender t => String -> ChannelParticipants -> Socket z t -> ZMQ z ()
reqMembers channel channels responder = do
  members <- liftIO (fetchChannelParticipants channels channel)
  send responder [] (C.toStrict . encode $ ResponseMembers { members })
