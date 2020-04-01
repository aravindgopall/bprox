module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as S
import Lib
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = proxy "localhost" "8080" "google.com" "80"

-- -| Take localhost localport remotehost remoteport
--    convention of local and remote is just to differentiate between listener
--    and original
-- -| listen on localhost <> localport
--    connect to remotehost <> remoteport
--    forward receive on remotehost socket to localhost socket
--    forward receive on localhost socket to remotehost socket
proxy :: HostName -> ServiceName -> HostName -> ServiceName -> IO ()
proxy localhost localport remotehost remoteport = do
  let hints = defaultHints {addrSocketType = Stream}
  -- listen on localhost <> localport
  addr:_ <- getAddrInfo (Just hints) (Just localhost) (Just localport)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  bind sock (addrAddress addr)
  listen sock 1 -- only for me
  forever $ do
    (sockL, sockAddr) <- accept sock
    -- connect to remotehost <> remoteport
    addrR:_ <- getAddrInfo (Just hints) (Just remotehost) (Just remoteport)
    sockR <-
      socket (addrFamily addrR) (addrSocketType addrR) (addrProtocol addrR)
    connect sockR (addrAddress addrR)
    putStrLn "connected Successfully to remote server"
    void $
      forkFinally (server sockR sockL) (\exception -> gracefulClose sockR 5000)
    void $
      forkFinally (server sockL sockR) (\exception -> gracefulClose sockR 5000)
  where
    server lSock rSock = do
      msg <- recv lSock 1024
      print msg
      unless (S.null msg) $ do
        sendAll rSock msg
        server lSock rSock
