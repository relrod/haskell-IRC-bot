import IRC
import System.IO (Handle)
import IRC.Proto
import IRC.Data

main :: IO ()
main = do
  conf <- loadConfig "./settings.cfg"
  connect conf $ (\c e -> return $ defaultEventListener c e) : onEvent : []

onEvent :: IRCConfig -> Event -> IO Action
onEvent conf (OnPrivmsg msg user to) = executeCommand (parseCommand msg) conf user to
onEvent _ _ = return Idle

executeCommand :: Args -> IRCConfig -> User -> Channel -> IO Action
executeCommand margs conf user chan = do
  args <- margs
  case args of
    ("hi":xs) -> reply ("Hi, " ++ (nick user)) conf user chan
    ("penis":xs) -> reply "8=====D" conf user chan
    _ -> return Idle
