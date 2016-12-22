{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings, LambdaCase #-}
module Parser
    (getCommands, isValidPage, htmlToFile, getNextLink, getCommand, parseComands
    ) where


import                Text.XML.HXT.Core
import                Text.HandsomeSoup
import                Network.HTTP.Conduit
import                Data.ByteString.Lazy as BSLazy (writeFile)
import                Data.Tree.NTree.TypeDefs
import                Data.Text hiding (foldr, length, head, filter)
import                Data.Maybe
import                Control.Monad


-- структура для хранения информации о git-команде
data Command = Command { title, description :: String }
  deriving (Show, Eq)

-- поиск команды по названию
searchCommand :: String -> [Command] -> Command
searchCommand title cmds = head $ filter (\(Command t d) -> t == title) cmds


rootURL  = "https://git-scm.com"
startURL = "https://git-scm.com/book/en/v2/Git-Commands-Setup-and-Config"

atTag tag = isElem //> css tag
text      = isElem //> getText
section   = isElem >>> css "section" >>> hasAttrValue "data-type" (== "sect2")


-- парсит команды из HXT
getCommand :: ArrowXml cat => cat XmlTree Command
getCommand = (atTag "section" >>> hasAttrValue "data-type" (== "sect2")) >>>
  proc x -> do
    ctitle <- text <<< atTag "h3" -< x
    tdescription <- listA $ text <<< atTag "p" -< x
    returnA -< Command { title = ctitle, description = foldr (++) "" tdescription}


-- ссылка на следующую страницу
getNextLink :: IOSLA (XIOState ()) XmlTree (NTree XNode) -> IO (Maybe String)
getNextLink html = do
  navLinks <- runX $ html >>> css "div"
                          >>> hasAttrValue "id" (== "nav")
                          //> css "a" ! "href"
  case length navLinks of
    2 -> return $ Just $ rootURL ++ navLinks !! 1
    _ -> return $ Nothing


-- сохраняет код страницы в файл
htmlToFile :: String -> FilePath -> IO ()
htmlToFile l fp = simpleHttp l >>= BSLazy.writeFile fp


-- создает DOM из html-файла
get :: FilePath -> IOStateArrow s b XmlTree
get fp = readDocument [ withParseHTML yes,
                        withInputEncoding unicodeString,
                        withWarnings no ] fp


-- проверка страницы на валидность
isValidPage :: IO Bool
isValidPage = do
  count <- runX $ (get "temp/site.html" >>> section //> hasName "h3") >. length
  return $ head count /= 0


-- парсит рекурсивно все команды из раздела Commands
parseComands :: String -> [Command] -> IO [Command]
parseComands curURL acc = do
  htmlToFile curURL "temp/site.html"
  validation <- isValidPage
  if validation
    then
      do
        commands <- runX $ get "temp/site.html" >>> getCommand
        nextURL  <- getNextLink $ get "temp/site.html"
        case nextURL of
          Just link -> parseComands link (acc ++ commands)
          Nothing   -> return $ acc ++ commands
    else
      return $ acc


-- result
getCommands :: IO [Command]
getCommands = parseComands startURL []
