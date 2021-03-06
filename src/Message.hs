{-# LANGUAGE DeriveGeneric #-}

module Message (
  MessageType(..),
  broadcast,
) where

import           Data.Aeson
import           GHC.Generics

-- | Messages to be serialized and sent to the client.
data MessageType
  -- | Initial message sent when a client first connects.
  = Hello { name :: String, channel :: String }
  -- | A text message sent from a client to all other clients.
  | Message { name :: String, channel :: String, content :: String }
  -- | A response of all members present in a given channel
  | ResponseMembers { members :: [String] }
  -- | Final message from a client, notifying the server, that the client is disconnecting.
  | Goodbye { name :: String, channel :: String }
  -- | Response from the server, containing a list of channels on the server.
  | ResponseChannels { channels :: [String] }
  deriving(Generic, Show)

broadcast = "broadcast" :: String

instance ToJSON MessageType where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON MessageType where
