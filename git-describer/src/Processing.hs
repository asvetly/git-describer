
module Processing
    (
    	get_text,
    	get_message,
    	input_processing,
    	delete_extra_symbols
    ) where

import           Web.Telegram.API.Bot
import           Web.Telegram.API.Bot.Responses
import           Data.Text as Text
import			 Parser
import			 Data.Char as Char
import			 Data.List as List

get_text :: Update -> Text
get_text x = get_text' (message x)
    where get_text' (Just mess) = get_text'' (text mess)
          get_text'' (Just x) = x

get_message :: Update -> [Command] -> Text
get_message x cmds = case (descByTitle (input_processing x) cmds) of
		        Just desc -> pack $ desc
		        otherwise -> pack $ "Error 404: command not found"


input_processing :: Update -> String
input_processing x = List.map Char.toLower $ delete_extra_symbols (unpack $ get_text x)

delete_extra_symbols :: String -> String
delete_extra_symbols = List.reverse . List.dropWhile notLetter . List.reverse . List.dropWhile notLetter
              where notLetter x = not $ isLetter x
