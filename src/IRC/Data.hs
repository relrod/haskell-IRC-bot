module IRC.Data (
  Channel,
  Message,
  Args,
  Event(..),
  Action(..),
  IRCConfig(..),
  User(..),
  Command(..)
) where

type Channel = String
type Message = String
type Args    = IO [String]

data Event = OnPing    Message -- code
           | OnCommand Command
           | OnPrivmsg Message User Channel
           | OnJoin    User Channel
           | OnPart    User Channel
           | OnInvite  User Channel
           | OnConnect
           | Unknown

data Action = Pong      Message -- code
            | Privmsg   Message Channel
            | Join      Channel
      --      | Part      Channel
      --      | Invite    User Channel
            | All [Action]
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

data Command = Command {
  args    :: Args,
  user    :: User,
  channel :: Channel
}
