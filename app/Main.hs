{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           System.ZMQ4.Monadic

main :: IO ()
main = runZMQ $ do
    -- Socket to talk to clients
    responder <- socket Rep
    bind responder "tcp://*:5555"

    publisher <- socket Pub
    bind publisher "tcp://*:6666"

    forever $ do
        buffer <- receive responder
        -- FIXME: Convert ByteString to String
        -- liftIO $ do
        --     putStrLn (B.append (B.pack "Received: ") buffer)
        send responder [] "ACK"

        send publisher [] buffer
