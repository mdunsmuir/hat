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

import Network.Socket

import Data.List
import Data.Word
import Data.Bits
import Hat.Acid
import Hat.Protocol

main = do
  sock <- socket AF_INET Stream 0
  hostAddress <- inet_addr "127.0.0.1"
  connect sock (SockAddrInet 4567 hostAddress)
  let msgs = [ProtocolMessage "foo bar" "bazman",
              ProtocolMessage "you suck" "yoker"]
  sendProtocol sock (MessagesResponse msgs)
  sClose sock
  return ()
