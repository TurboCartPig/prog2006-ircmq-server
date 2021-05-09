{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as C
import qualified Data.Map              as Map
import           Handlers
import           Message
import           System.ZMQ4.Monadic

-- | handleMessageType - handles the type of message and sends it to the appropriate function
handleMessageType :: (Sender t, Sender t2) => B.ByteString -> Socket z t -> Socket z t2 -> ChannelParticipants -> ZMQ z ()
handleMessageType buffer responder publisher channels = do
  -- Deserialize the message into a MessageType
  case (decodeStrict buffer :: Maybe MessageType) of
    Just (Hello   name channel)         -> Handlers.hello name channel responder publisher channels
    Just (Goodbye name channel)         -> Handlers.goodbye name channel responder publisher channels
    Just (Message name channel content) -> Handlers.message name channel content responder publisher
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

