{-
    Copyright (C) 2015  Michael Dunsmuir

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

import Control.Concurrent
import Control.Concurrent.Chan
import Network.Socket
import Data.Acid

import Hat.Message
import Hat.Protocol

main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 4567 iNADDR_ANY)
  listen sock 2

  --acid <- openLocalState initialState
  clientThread sock

{-
newClientThread :: Socket -> Chan Message -> IO (ThreadID, Chan Message)
newClientThread sock outChan = do
-}

clientThread :: Socket -> IO ()
clientThread sock = do
  conn <- accept sock
  runConnection conn
  clientThread sock

runConnection :: (Socket, SockAddr) -> IO ()
runConnection (sock, addr) = do
  eitherMessage <- recvProtocol sock 
  let msg = case eitherMessage of
        Left err -> err
        Right pro -> show pro
  putStrLn msg
  sClose sock
