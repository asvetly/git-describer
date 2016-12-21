module Messages
    ( 
      token,
      size,
      main_loop,
      getMsg,
      getMsgFunction,
      get_update_id,
      get_text 
    ) where
    

{-# LANGUAGE OverloadedStrings #-}

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import           Web.Telegram.API.Bot.Responses
import           Data.Maybe
import           Data.Text
import           Data.List as List
import           Data.String

token :: Token
token = Token $ pack "bot281687116:AAHGdr5AP7_96pE-75_UoQLILZqGRzMeUkg" 

size :: Maybe Int
size = Just 20

main_loop :: IO()
main_loop = do 
    getMsg Nothing
    main_loop

getMsg :: Maybe Int -> IO()
getMsg id = do
  manager <- newManager tlsManagerSettings
  res <- getUpdates token id size Nothing manager
  case res of
    Left e -> do
      putStrLn "Request failed"
      print e
    Right (Response m) -> do
      getMsgFunction m

getMsgFunction :: [Update] -> IO ()      
getMsgFunction [] = do putStr ""
getMsgFunction m = do 
    processing m
    getMsg (Just((get_update_id m)+1))


processing :: [Update] -> IO ()
processing [] = putStr ""
processing (x:xs)= do 
    putStr "Accept"
    processing xs
    
get_update_id :: [Update] -> Int     
get_update_id m = update_id (List.last m) 

get_text :: Update -> Text
get_text x = get_text' (message x)
    where get_text' (Just mess) = get_text'' (text mess)
          get_text'' (Just x) = x    