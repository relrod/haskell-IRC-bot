import Network
import System.IO
import Text.Printf
import Data.List
import Data.List.Split
import Data.ConfigFile
import Data.Either.Utils
import Control.Monad

type Channel = String
type Message = String

data Event = Ping    Message -- code
           | Privmsg Message User Channel
           | Join    User Channel
           | Part    User Channel
           | Invite  User Channel
           | Unknown

data IRCConfig = IRCConfig {
  server    :: String,
  port      :: Int,
  chans     :: [Channel],
  username  :: String,
  realname  :: String
}

data User = User {
  nick :: String,
  name :: String,
  host :: String,
  raw  :: String
}

decodeUser :: String -> User
decodeUser s = User {
    nick = takeWhile (/= '!') s,
    name = takeWhile (/= '@') $ dropWhile (/= '!') s,
    host = dropWhile (/= '@') s,
    raw  = s
  }

main = do
  conf <- loadConfig "./settings.cfg"
  h <- connectTo (server conf) (PortNumber (fromIntegral (port conf)))
  hSetBuffering h NoBuffering
  write h $ "NICK " ++ username conf
  write h $ "USER " ++ username conf ++ " 0 * :" ++ realname conf
  write h $ "JOIN " ++ head (chans conf)
  listen h

loadConfig :: String -> IO IRCConfig
loadConfig path = do
    val <- readfile emptyCP path
    let cp = forceEither val
    return IRCConfig {
      server   = forceEither $ get cp "DEFAULT" "server",
      port     = forceEither $ get cp "DEFAULT" "port",
      chans    = splitOn " " $ forceEither $ get cp "DEFAULT" "channels",
      username = forceEither $ get cp "DEFAULT" "username",
      realname = forceEither $ get cp "DEFAULT" "realname"
    }

write :: Handle -> String -> IO ()
write h = hPrintf h "%s\r\n"

listen :: Handle -> IO ()
listen h = forever $ do
    t <- hGetLine h
    let s = init t
    onEvent h $ readCommand s
    putStrLn s

readCommand :: String -> Event
readCommand s
    | "PING :"  `isPrefixOf` s   = Ping $ drop 6 s
    | "PRIVMSG" `isInfixOf`  s   = Privmsg (getMessage s) (getUser s) (getMessageReiceiver s)
    | "JOIN"    `isInfixOf`  s   = Join (getUser s) (getMessage s)
    | "PART"    `isInfixOf`  s   = Part (getUser s) (getPartedUser s)
    | "INVITE"  `isInfixOf`  s   = Invite (getUser s) (getMessage s)
    | otherwise                   = Unknown
  where
    getUser = decodeUser . drop 1 . takeWhile (' ' /=)
    getMessage = drop 1 . dropWhile (':' /=) . drop 1
    getMessageReiceiver = takeWhile (' ' /=) . drop 9 . dropWhile (' ' /=)
    getPartedUser = drop 6 . takeWhile (' ' /=)

onEvent :: Handle -> Event -> IO ()
onEvent h (Ping code) = do
  write h $ "PONG :" ++ code
  return ()
onEvent h (Privmsg msg user to) = do
  printf "%s\n" $ "Received message \""++msg++"\" by "++ nick user ++" to "++to
  return ()
onEvent h (Invite user channel) = do
  write h $ "JOIN " ++ channel
  return ()
onEvent _ _           = return ()
