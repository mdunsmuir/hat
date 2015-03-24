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

{-# LANGUAGE DeriveDataTypeable #-}

import System.Environment
import Network.Socket
import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Typeable
import Data.IORef
import Data.Char (isPrint)
import Data.List
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified UI.HSCurses.Curses as C
import qualified UI.HSCurses.CursesHelper as CH

import Hat.Message
import Hat.Protocol

data InputException = Quit deriving (Show, Typeable)
instance Exception InputException

main = do
  args <- getArgs
  case args of
    [user, host, port] -> do
      socket <- connectSocket host $ fromIntegral (read port :: Int)
      runClient user socket
      sClose socket
    _ -> putStrLn "usage: hat-client <username> <host> <port>"

-- | Make a socket and connect it to the given host name and port, if available.
connectSocket :: String -> PortNumber -> IO Socket
connectSocket host port = do
  let hint = defaultHints { addrFamily = AF_INET, addrSocketType = Stream }
  addrInfos <- getAddrInfo (Just hint) (Just host) Nothing
  let info = if null addrInfos then Nothing else Just $ head addrInfos
  case info of
    Just (AddrInfo _ _ _ _ (SockAddrInet _ addr) _) -> do
      sock <- socket AF_INET Stream 0
      connect sock (SockAddrInet port addr)
      return sock
    _ -> error "could not find host"

runClient :: String -> Socket -> IO ()
runClient user sock = do
  CH.start
  C.cursSet C.CursorInvisible
  let window = C.stdScr
  mWindow <- newMVar window
  showId <- forkIO $ showMessages sock mWindow
  catch (takeInput user sock mWindow) $ \e ->
    case e of
      Quit -> return ()
  killThread showId
  CH.end

-- showing messages

showMessages :: Socket -> MVar C.Window -> IO ()
showMessages sock mWindow = do
  msgs <- newIORef S.empty
  forever $ do
    withMVar mWindow (drawMessages msgs)
    incoming <- recvProtocol sock  
    case incoming of
      Left err -> error err
      Right (MessageRequest msg) -> modifyIORef msgs (msg S.<|)

drawMessages :: IORef (S.Seq Message) -> C.Window -> IO ()
drawMessages msgs window = do
  (rows, cols) <- C.scrSize 
  let drawOrder = reverse [0..(rows - 3)]
  msgs' <- (F.toList . S.take (rows - 2)) <$> readIORef msgs
  F.forM_ (zip msgs' drawOrder) $ \((Message msg user), row) -> do
    C.wMove window row 0
    let userStr = user ++ " >> "
        msgStr = take (cols - 1 - length userStr) msg
        fullStr = userStr ++ msgStr ++ 
                    replicate (cols - 1 - (length userStr + length msgStr)) ' '
    C.wAddStr window fullStr
  C.wRefresh window

-- taking input

takeInput :: String -> Socket -> MVar C.Window -> IO ()
takeInput user sock mWindow = do
  input <- newIORef S.empty
  forever $ do
    withMVar mWindow (drawInput input)
    key <- C.getCh
    case key of
      C.KeyChar '\ETX' -> throwIO Quit
      C.KeyChar '\DEL' -> modifyIORef input backspace
      C.KeyChar '\r' -> do
        inputStr <- F.toList <$> readIORef input
        writeIORef input S.empty
        sendMessage sock $ Message inputStr user
      C.KeyChar c -> when (isPrint c) $ modifyIORef input (S.|> c)
  where
    backspace input =
      case S.viewr input of
        S.EmptyR -> input
        input' S.:> _ -> input'

drawInput :: IORef (S.Seq Char) -> C.Window -> IO ()
drawInput ioInput window = do
  (rows, cols) <- C.scrSize
  input <- readIORef ioInput
  let (_, toDrawInput) = S.splitAt (max (S.length input - cols + 1) 0) input
  C.wMove window (rows - 2) 0
  C.wAddStr window $ replicate (cols - 1) '_'
  C.wMove window (rows - 1) 0
  C.wAddStr window $ (F.toList toDrawInput) ++ replicate (cols - S.length toDrawInput - 1) ' '
  C.wRefresh window

sendMessage :: Socket -> Message -> IO ()
sendMessage sock msg  = do
  sendProtocol sock $ MessageRequest msg
  return ()
