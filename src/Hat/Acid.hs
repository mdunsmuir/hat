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

{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

module Hat.Acid (
  AcidMessages
, InsertMessage(..)
, LastNMessages(..)
, initialState
) where

import Control.Monad.State
import Control.Monad.Reader
import Data.Typeable
import qualified Data.Sequence as S
import Data.Acid
import Data.SafeCopy
import Hat.Message

data AcidMessages = AcidMessages (S.Seq Message)
                    deriving Typeable

$(deriveSafeCopy 0 'base ''AcidMessages)

insertMessage :: Message -> Update AcidMessages ()
insertMessage msg = do
  AcidMessages seq <- get
  put $ AcidMessages $ msg S.<| seq

lastNMessages :: Int -> Query AcidMessages (S.Seq Message)
lastNMessages n = do
  AcidMessages msgs <- ask
  return $ S.take n msgs

$(makeAcidic ''AcidMessages ['insertMessage, 'lastNMessages])

initialState :: AcidMessages
initialState = AcidMessages S.empty
