import IRC
import Network
import System.IO

main :: IO ()
main = do
  conf <- loadConfig "./settings.cfg"
  initializeIRC conf onEvent

onEvent :: Handle -> Event -> IO()
onEvent h (Privmsg msg user to) | startsWith' msg "!hi" = hi h to user
                                | otherwise = return ()
  where
    hi h channel user = write h $ "PRIVMSG " ++ channel ++ " :" ++ (nick user)
onEvent _ _ = return ()

startsWith' :: String -> String -> Bool
startsWith' s t = take (length t) s == t
