{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           GHC.Generics
import qualified Data.Map as Map
import           System.ZMQ4.Monadic

-- | Messages to be serialized and sent to the client.
data MessageType
  -- | Initial message sent when a client first connects.
  = Hello { name :: String, channel :: String }
  -- | A text message sent from a client to all other clients.
  | Message { name :: String, channel :: String, content :: String }
  -- | Final message from a client, notifying the server, that the client is disconnecting.
  | Goodbye { name :: String, channel :: String }
  deriving(Generic, Show)

instance ToJSON MessageType where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON MessageType where

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
        send responder [] "ACK"
        
        let json = decodeStrict buffer :: Maybe MessageType

        case json of
          Just (Hello   name ch) -> liftIO $ insertChannelParticipant channels name ch
          Just (Message name ch content) -> do
            liftIO (putStrLn $ name ++ ": " ++ content)
            -- Publish the message for all clients to see, on the topic/channel "A"
            send publisher [SendMore] (B.pack ch)
            send publisher [] (B.pack $ name ++ ": " ++ content)
          _ -> liftIO (putStrLn "NOT A MESSAGE")


insertChannelParticipant :: ChannelParticipants -> String -> String  -> IO () 
insertChannelParticipant (ChannelParticipants cp) name ch = do 
  channels <- takeMVar cp
  let res = Map.lookup ch channels
  case res of 
    Just chn -> do 
      let new_channel = name : chn
      let channels' = Map.insert ch new_channel channels
      putMVar cp channels'
      putStrLn ("New participant added to the old channel: " ++ ch)
    Nothing  -> do 
      let channels' = Map.insert ch [name] channels
      putMVar cp channels'
      putStrLn ("New participant added to the new channel: " ++ ch)
  
  
newChannelMap :: IO ChannelParticipants 
newChannelMap = do 
  m <- newMVar (Map.empty)
  return (ChannelParticipants m)