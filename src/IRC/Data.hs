module IRC.Data (
  Channel,
  Message,
  Event(..),
  Action(..),
  IRCConfig(..),
  User(..)
) where

type Channel = String
type Message = String

data Event = OnPing    Message -- code
           | OnPrivmsg Message User Channel
           | OnJoin    User Channel
           | OnPart    User Channel
           | OnInvite  User Channel
           | Unknown
           
data Action = Pong      Message -- code
            | Privmsg   Message Channel
            | Join      Channel
      --      | Part      Channel
      --      | Invite    User Channel
            | Idle

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
