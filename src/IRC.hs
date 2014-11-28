module IRC (
  loadConfig,
  defaultEventListener,
  parseCommand,
  reply
) where
  
import System.IO (Handle)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.ConfigFile
import Data.Either.Utils (forceEither)
import Control.Applicative ((<$>))
import IRC.Proto
import IRC.Data

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

defaultEventListener :: IRCConfig -> Event -> Action
defaultEventListener _ (OnPing code)           = Pong code
defaultEventListener _ (OnInvite user channel) = Join channel
defaultEventListener c (OnConnect)             = All $ (\c -> Join c) <$> chans c
defaultEventListener _ _                       = Idle

parseCommand :: Message -> Args
parseCommand msg = do
  if "!" `isPrefixOf` msg then
    return $ (splitOn " " . drop 1) msg
  else return []

reply :: Message -> IRCConfig -> User -> Channel -> IO Action
reply msg conf user channel | channel == (username conf) = return $ Privmsg msg (nick user)
                            | otherwise = return $ Privmsg msg channel
