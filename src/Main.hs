import IRC
import System.IO (Handle)
import Data.List (isInfixOf, isPrefixOf)
import IRC.Proto
import IRC.Data

main :: IO ()
main = do
  conf <- loadConfig "./settings.cfg"
  connect conf [defaultEventListener, onEvent]

onEvent :: Event -> Action
onEvent (OnPrivmsg msg user to) | "!hi" `isPrefixOf` msg = Privmsg ("Hi, " ++ (nick user)) to
                                | otherwise = Idle
onEvent _ = Idle
