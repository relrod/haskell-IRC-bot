module IRC.Proto (
  write,
  connect
) where

import Text.Printf (hPrintf)
import Control.Monad (forever, foldM_)
import Control.Applicative ((<*>), (<$>))
import Network (connectTo, PortID(..))
import System.IO (hSetBuffering, BufferMode(NoBuffering), Handle, hGetLine)
import Data.List (isInfixOf, isPrefixOf)
import IRC.Data (User(..), Event(..), IRCConfig(..), Action(..))
  
decodeUser :: String -> User
decodeUser s = User {
    nick = takeWhile (/= '!') s,
    name = takeWhile (/= '@') $ dropWhile (/= '!') s,
    host = dropWhile (/= '@') s,
    raw  = s
  }

readCommand :: String -> Event
readCommand s
    | "PING :"  `isPrefixOf` s   = OnPing $ drop 6 s
    | "PRIVMSG" `isInfixOf`  s   = OnPrivmsg (getMessage s) (getUser s) (getMessageReiceiver s)
    | "JOIN"    `isInfixOf`  s   = OnJoin (getUser s) (getMessage s)
    | "PART"    `isInfixOf`  s   = OnPart (getUser s) (getPartedUser s)
    | "INVITE"  `isInfixOf`  s   = OnInvite (getUser s) (getMessage s)
    | otherwise                  = Unknown
  where
    getUser = decodeUser . drop 1 . takeWhile (' ' /=)
    getMessage = drop 1 . dropWhile (':' /=) . drop 1
    getMessageReiceiver = takeWhile (' ' /=) . drop 9 . dropWhile (' ' /=)
    getPartedUser = drop 6 . takeWhile (' ' /=)

write :: Handle -> String -> IO ()
write h = hPrintf h "%s\r\n"

listen :: Handle -> [Event -> Action] -> IO ()
listen h eventListeners = forever $ do
    t <- hGetLine h
    let s = init t
    let event = readCommand s
    let actions = eventListeners <*> [event]
    foldM_ performAction h actions
    putStrLn s

connect :: IRCConfig -> [Event -> Action] -> IO ()
connect conf eventListeners = do
  h <- connectTo (server conf) (PortNumber (fromIntegral (port conf)))
  hSetBuffering h NoBuffering
  write h $ "NICK " ++ username conf
  write h $ "USER " ++ username conf ++ " 0 * :" ++ realname conf
  write h $ "JOIN " ++ head (chans conf)
  listen h eventListeners

performAction :: Handle -> Action -> IO Handle
performAction h (Pong code) = do
  write h $ "PONG :" ++ code
  return h
performAction h (Privmsg message channel) = do
  write h $ "PRIVMSG " ++ channel ++ " :" ++ message
  return h
performAction h (Join channel) = do
  write h $ "JOIN " ++ channel
  return h
performAction h _ = return h
