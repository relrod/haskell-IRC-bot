module IRC.Proto (
  write,
  connect
) where

import Text.Printf (hPrintf)
import Control.Monad (forever)
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
    | " 001 "   `isInfixOf`  s   = OnConnect
    | otherwise                  = Unknown
  where
    getUser = decodeUser . drop 1 . takeWhile (' ' /=)
    getMessage = drop 1 . dropWhile (':' /=) . drop 1
    getMessageReiceiver = takeWhile (' ' /=) . drop 9 . dropWhile (' ' /=)
    getPartedUser = drop 6 . takeWhile (' ' /=)

write :: Handle -> String -> IO ()
write h = hPrintf h "%s\r\n"

listen :: Handle -> IRCConfig -> (IRCConfig -> Event -> IO Action) -> IO ()
listen h conf eventListener = forever $ do
    s <- hGetLine h
    let event = readCommand (init s)
    action <- eventListener conf event
    performAction h action
    putStrLn s

connect :: IRCConfig -> (IRCConfig -> Event -> IO Action) -> IO ()
connect conf eventListener = do
  h <- connectTo (server conf) (PortNumber (fromIntegral (port conf)))
  hSetBuffering h NoBuffering
  write h $ "NICK " ++ username conf
  write h $ "USER " ++ username conf ++ " 0 * :" ++ realname conf
  listen h conf eventListener

performAction :: Handle -> Action -> IO ()
performAction h (Pong code) =
  write h $ "PONG :" ++ code
performAction h (Privmsg message channel) =
  write h $ "PRIVMSG " ++ channel ++ " :" ++ message
performAction h (Join channel) =
  write h $ "JOIN " ++ channel
performAction h (All actions) =
  mapM_ (performAction h) actions
performAction _ _ = return ()
