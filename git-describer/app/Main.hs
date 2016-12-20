{-# LANGUAGE OverloadedStrings #-}

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import           Web.Telegram.API.Bot.Responses



main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  res <- getMe token manager
  case res of
    Left e -> do
      putStrLn "Request failed"
      print e
    Right (Response u) -> do
      putStrLn "Request succeded"
      print $ user_first_name u
  where token = Token "bot281687116:AAHGdr5AP7_96pE-75_UoQLILZqGRzMeUkg" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
