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

type ChannelParticipants = Map.Map String [String]

main :: IO ()
main = runZMQ $ do
    -- Socket to talk to individual chat clients
    responder <- socket Rep
    bind responder "tcp://*:5555"

    -- Socket to publish new messages to all chat clients
    publisher <- socket Pub
    bind publisher "tcp://*:6666"

    forever $ do
        -- Get new message from a client
        buffer <- receive responder

        let json = decodeStrict buffer :: Maybe MessageType

        case json of
          Just (Message name ch content) -> liftIO (putStrLn $ "Received: " ++ content)
          _ -> liftIO (putStrLn "NOT A MESSAGE")


        send responder [] "ACK"

        -- Publish the message for all clients to see, on the topic/channel "A"
        send publisher [SendMore] "A"
        send publisher [] buffer
