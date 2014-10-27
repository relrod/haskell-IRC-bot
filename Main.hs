import Network
import System.IO
import Text.Printf
import Data.List
import Data.List.Split
import Data.ConfigFile
import Data.Either.Utils

data Event = Ping    String   -- code
           | Privmsg String   -- msg
           | Join
           | Unknown

data IRCConfig = IRCConfig {
  server    :: String,
  port      :: Int,
  chans     :: [String],
  username  :: String,
  realname  :: String
}

main = do
  conf <- loadConfig "./settings.cfg"
  h <- connectTo (server conf) (PortNumber (fromIntegral (port conf)))
  hSetBuffering h NoBuffering
  write h $ "NICK " ++ (username conf)
  write h $ "USER " ++ (username conf) ++ " 0 * :" ++ (realname conf)
  write h $ "JOIN " ++ (head $ chans conf)
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
write h s = hPrintf h "%s\r\n" s

listen :: Handle -> IO ()
listen h = forever $ do
    t <- hGetLine h
    let s = init t
    onEvent h $ readCommand s
    putStrLn s
  where
    forever a = a >> forever a

readCommand :: String -> Event
readCommand s
    | ("PING :"  `isPrefixOf` s)   = Ping $ drop 6 s
    | ("PRIVMSG" `isInfixOf`  s)   = Privmsg $ getMessage s
    | otherwise                    = Unknown
  where
    getMessage s = drop 1 $ dropWhile (/=':') s

onEvent :: Handle -> Event -> IO ()
onEvent h (Ping code) = do
  write h $ "PONG :" ++ code
  return ()
onEvent _ _           = return ()
