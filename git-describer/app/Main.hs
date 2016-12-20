{-# LANGUAGE OverloadedStrings #-}

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import           Web.Telegram.API.Bot.Responses

getMsg = do
  manager <- newManager tlsManagerSettings
  let request = sendMessageRequest chatId message
  res <- sendMessage token request manager
  case res of
    Left e -> do
      putStrLn "Request failed"
      print e
    Right (Response m) -> do
      putStrLn "Request succeded"
      print $ message_id m
      print $ text m
  getMsg
  where token = Token "bot281687116:AAHGdr5AP7_96pE-75_UoQLILZqGRzMeUkg" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
        chatId = "205838843"
        message = "Данька лах)))0))"

main :: IO ()
main = getMsg
