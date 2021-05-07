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


        let json = decodeStrict buffer :: Maybe MessageType

        case json of
          Just (Hello   name ch) -> do
            send responder [] "ACK"
            liftIO $ insertChannelParticipant channels name ch
          Just (Message n ch cont) -> do
            send responder [] "ACK"
            liftIO (putStrLn $ n ++ ": " ++ cont)
            -- Publish the message for all clients to see, on the topic/channel 'ch'
            send publisher [SendMore] (B.pack ch)
            send publisher [] (C.toStrict (encode (Message { name = n, channel = ch, content = cont})))
          Just (RequestMembers ch) -> do
            participants <- liftIO (fetchChannelParticipants channels ch)
            send responder [] (C.toStrict ( encode (ResponseMembers {members = participants})))
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

fetchChannelParticipants :: ChannelParticipants -> String -> IO [String]
fetchChannelParticipants (ChannelParticipants cp) ch = do
  channels <- takeMVar cp
  let res = Map.lookup ch channels
  case res of
    Just chn -> return chn
    Nothing  -> return []
  
newChannelMap :: IO ChannelParticipants 
newChannelMap = do 
  m <- newMVar (Map.empty)
  return (ChannelParticipants m)