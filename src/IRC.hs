module IRC (
  loadConfig,
  defaultEventListener
) where
  
import System.IO (Handle)
import Data.List.Split (splitOn)
import Data.ConfigFile
import Data.Either.Utils (forceEither)
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

defaultEventListener :: Event -> Action
defaultEventListener (OnPing code) = Pong code
defaultEventListener (OnInvite user channel) = Join channel
defaultEventListener _           = Idle
