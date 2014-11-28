import IRC
import System.IO (Handle)
import IRC.Proto
import IRC.Data

main :: IO ()
main = do
  conf <- loadConfig "./settings.cfg"
  connect conf onEvent

onEvent :: IRCConfig -> Event -> IO Action
onEvent conf (OnPrivmsg msg user to) = executeCommand (parseCommand msg) conf user to
onEvent conf event = return $ defaultEventListener conf event

executeCommand :: Args -> IRCConfig -> User -> Channel -> IO Action
executeCommand margs conf user chan = do
  args <- margs
  case args of
    ("hi":xs) -> return $ reply ("Hi, " ++ (nick user)) conf user chan
    ("penis":xs) -> return $ reply "8=====D" conf user chan
    _ -> return Idle
