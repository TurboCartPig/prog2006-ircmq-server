{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           System.ZMQ4.Monadic

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

        liftIO (putStrLn $ B.unpack buffer)

        send responder [] "ACK"

        -- Publish the message for all clients to see, on the topic/channel "A"
        send publisher [SendMore] "A"
        send publisher [] buffer
