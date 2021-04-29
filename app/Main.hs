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
        liftIO (putStrLn $ B.unpack buffer)

        send responder [] "ACK"

        send publisher [SendMore] "A"
        send publisher [] buffer
