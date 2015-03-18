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

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Network.Socket

import Hat.Message
import Hat.Protocol

main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 4567 iNADDR_ANY)
  listen sock 3
  relayChan <- newChan :: IO (Chan Message)
  acceptLoop sock relayChan

acceptLoop :: Socket -> Chan Message -> IO ()
acceptLoop sock relayChan = forever $ do
  (sock, _) <- accept sock
  newClientThread sock relayChan

newClientThread :: 
  Socket -> Chan Message -> IO ()
newClientThread sock relayChan = do
  forkIO $ fromClientThread sock relayChan
  outChan <- dupChan relayChan
  forkFinally (toClientThread sock outChan) $ \result -> do
    case result of
      Left exc -> return () -- putStrLn $ show exc
      Right _ -> return ()
    sClose sock
  return ()

fromClientThread :: Socket -> Chan Message -> IO ()
fromClientThread sock relayChan = do
  eitherMessage <- recvProtocol sock
  case eitherMessage of
    Left err -> return () --putStrLn err
    Right (MessageRequest msg) -> do
      writeChan relayChan msg
      fromClientThread sock relayChan

toClientThread :: Socket -> Chan Message -> IO ()
toClientThread sock inChan = forever $ do
  msg <- readChan inChan
  sendProtocol sock $ MessageRequest msg
