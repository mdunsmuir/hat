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

module Hat.Client where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.MVar
import Network.Socket
import UI.NCurses
import Data.Char
import qualified Data.Foldable as F
import qualified Data.Sequence as S

import Hat.Message
import Hat.Protocol

data ClientState = ClientState { clientMessages :: S.Seq Message
                               , textBuffer :: S.Seq Char
                               , clientWindow :: Window }

runClient :: Socket -> IO ()
runClient sock = do


runClient :: Socket -> Curses ()
runClient sock = do
  setRaw True
  setEcho False
  window <- defaultWindow
  let client = ClientState S.empty S.empty window
  drawMessages client window
  drawInput client window
  mClient <- liftIO $ newMVar $ client
  liftIO $ forkIO $ receiveMessages sock mClient
  eventLoop sock window mClient

eventLoop :: Socket -> Window -> MVar ClientState -> Curses ()
eventLoop sock window mClient = do
  maybeEvent <- getEvent window Nothing
  case maybeEvent of
    Just (EventCharacter '\ETX') -> return ()

    Just (EventCharacter '\n') -> do
      ClientState msgs tb window' <- liftIO $ takeMVar mClient 
      let msg = Message (F.toList tb) "some asshole"
          client' = ClientState msgs S.empty window'
      drawInput client' window'
      liftIO $ putMVar mClient client'
      sendResult <- liftIO $ sendProtocol sock (MessageRequest msg)
      case sendResult of
        Left err -> return ()
        Right _ -> eventLoop sock window' mClient

    Just (EventCharacter c) -> 
      if not (isPrint c) then eventLoop sock window mClient
        else do
          ClientState msgs tb window' <- liftIO $ takeMVar mClient 
          let tb' = tb S.|> c
              client' = ClientState msgs tb' window'
          drawInput client' window'
          liftIO $ putMVar mClient client'
          eventLoop sock window mClient

    _ -> eventLoop sock window mClient
                          

receiveMessages :: Socket -> MVar ClientState -> IO ()
receiveMessages sock mClient = forever $ do
  eitherMsg <- recvProtocol sock
  case eitherMsg of
    Left _ -> fail "lost connection"
    Right (MessageRequest msg) -> do
      ClientState msgs tb window <- takeMVar mClient
      let client' = ClientState (msg S.<| msgs) tb window
      runCurses $ drawMessages client' window
      putMVar mClient client'
      
displayMessage :: Message -> String
displayMessage (Message msg user) = user ++ " >> " ++ msg

drawMessages :: ClientState -> Window -> Curses ()
drawMessages client window = do
  (rows, cols) <- screenSize 
  let drawHeight = rows - 2
      msgs = F.toList $ S.take (fromInteger drawHeight) $ clientMessages client
  forM_ (zip [0..] msgs) $ \(row, msg) -> do
    updateWindow window $ moveCursor row 0
    updateWindow window $ drawString $ take (fromInteger (cols - 1)) $ displayMessage msg
  render

drawInput :: ClientState -> Window -> Curses ()
drawInput client window = do
  (rows, cols) <- screenSize
  updateWindow window $ moveCursor (rows - 2) 0
  updateWindow window $ drawString $ replicate (fromInteger (cols - 1)) '-'
  updateWindow window $ moveCursor (rows - 1) 0
  let input = take (fromInteger (cols - 1)) $ F.toList $ textBuffer client
      input' = if length input < fromInteger (cols - 1)
                 then input ++ replicate (fromInteger (cols - 1) - length input) ' '
                 else input'
  updateWindow window $ drawString input'
  render
