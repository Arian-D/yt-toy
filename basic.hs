{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

import GHC.Generics
import Data.Aeson
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)
import System.Process (callCommand)

youtubify :: String -> String
youtubify videoId = "https://youtube.com/watch?v=" ++ videoId

search :: (MonadIO m, MonadThrow m) => String -> m  L8.ByteString
search query =
  getResponseBody <$> (parseRequest uri >>= httpLBS)
  where uri = "https://invidious.kavin.rocks/api/v1/search?q=" ++ query

data SearchResult = SearchResult
  { title :: String
  , videoId :: String
  -- , description :: String
  -- , author :: String
  } deriving (Generic, Show)
  
-- TODO: Manual parse to detect type
instance FromJSON SearchResult 

main :: IO ()
main = do
  result <- search "belle delphine"
  let decoded = decode result :: Maybe [SearchResult]
  case decoded of Just l -> mapM_ print (zip [0..] l)
                  Nothing -> putStrLn "rip"
  input <- getLine
  let number = read input :: Int -- Change to readEither or readMaybe
  let id = fmap (\r -> videoId r) $ (!!number) <$> decoded
  let url = fmap (youtubify) id
  case url of Just link -> callCommand ("mpv " ++ link)
              Nothing -> putStrLn "oh well"
  
