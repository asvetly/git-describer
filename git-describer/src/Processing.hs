
module Processing
    (
    	get_text,
    	get_message
    ) where

import           Web.Telegram.API.Bot
import           Web.Telegram.API.Bot.Responses
import           Data.Text
import			 Parser

get_text :: Update -> Text
get_text x = get_text' (message x)
    where get_text' (Just mess) = get_text'' (text mess)
          get_text'' (Just x) = x

get_message :: Update -> [Command] -> Text
get_message x cmds = case (descByTitle (unpack $ get_text x) cmds) of
		        Just desc -> pack $ desc
		        otherwise -> pack $ "Error 404: command not found"