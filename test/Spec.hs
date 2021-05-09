{-# LANGUAGE NamedFieldPuns    #-}
import Test.Hspec
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as C
import Message

main :: IO ()
main = hspec $ do
  serialize_json_to_string

serialize_json_to_string :: Spec
serialize_json_to_string = do
  let name = "Bob"
  let channel = "Channel #1"
  let content = "Hello world!"
  let members = ["Bob","Dave"]
  let channels = ["Channel #1", "Channel #2"]
  describe "Serializing JSON to strings" $ do
    it "Works for Hello" $ do
      show (encode $ Hello {name, channel}) `shouldBe` "\"{\\\"tag\\\":\\\"Hello\\\",\\\"name\\\":\\\"Bob\\\",\\\"channel\\\":\\\"Channel #1\\\"}\""
    it "Works for Message" $ do
      show (encode $ Message {name, channel,content}) `shouldBe` "\"{\\\"tag\\\":\\\"Message\\\",\\\"name\\\":\\\"Bob\\\",\\\"channel\\\":\\\"Channel #1\\\",\\\"content\\\":\\\"Hello world!\\\"}\""
    it "Works for ResponseMembers" $ do
      show (encode $ ResponseMembers {members}) `shouldBe` "\"{\\\"tag\\\":\\\"ResponseMembers\\\",\\\"members\\\":[\\\"Bob\\\",\\\"Dave\\\"]}\""
    it "Works for Goodbye" $ do
      show (encode $ Goodbye {name,channel}) `shouldBe` "\"{\\\"tag\\\":\\\"Goodbye\\\",\\\"name\\\":\\\"Bob\\\",\\\"channel\\\":\\\"Channel #1\\\"}\""
    it "Works for ResponseChannels" $ do
      show (encode $ ResponseChannels {channels}) `shouldBe` "\"{\\\"tag\\\":\\\"ResponseChannels\\\",\\\"channels\\\":[\\\"Channel #1\\\",\\\"Channel #2\\\"]}\""
