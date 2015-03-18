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

{-# LANGUAGE DeriveGeneric #-}

module Hat.Protocol where

import GHC.Generics
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Serialize (Serialize, encode, decode)
import qualified Data.ByteString as B

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Hat.Message

data Protocol = MessageRequest { message :: Message }
              | HistoryRequest { historyLength :: Int }
              | MessagesResponse { messages :: [Message] }
                deriving (Generic, Show)

instance Serialize Protocol where

-- | Recieve a `Protocol` from the given `Socket`.
recvProtocol :: Socket -> IO (Either String Protocol)
recvProtocol sock = runEitherT $ do
  msgLenBytes <- liftIO $ recv sock 8 
  when (B.length msgLenBytes /= 8) $ left "err 1"
  msgLen <- hoistEither $ decode msgLenBytes :: EitherT String IO Int
  msgBytes <- liftIO $ recv sock msgLen
  when (B.length msgBytes /= msgLen) $ left "err 2"
  msg <- hoistEither $ decode msgBytes :: EitherT String IO Protocol
  return msg

-- | Send a `Protocol` to the given `Socket`.
sendProtocol :: Socket -> Protocol -> IO (Either String ())
sendProtocol sock msg = runEitherT $ do
  let msgBytes = encode msg
      msgBytesLen = B.length msgBytes
  bytesSent <- liftIO $ send sock $ encode msgBytesLen
  when (bytesSent /= 8) $ left "err 3"
  bytesSent' <- liftIO $ send sock msgBytes
  when (bytesSent' /= msgBytesLen) $ left "err 4"
  return ()
