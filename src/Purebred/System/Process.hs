-- This file is part of purebred
-- Copyright (C) 2019 Róman Joost
--
-- purebred is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Purebred.System.Process
  ( tryRunProcess
  , handleIOException
  , handleExitCode
  ) where

import System.Exit (ExitCode(..))
import Control.Exception (try, IOException)
import System.Process.Typed (runProcess, ProcessConfig)
import Control.Lens (set, (&))

import Error
import Types


-- | Handler to handle exit failures setting an error in the AppState
handleExitCode :: AppState -> ExitCode -> AppState
handleExitCode s (ExitFailure e) = s & setError (GenericError (show e))
handleExitCode s ExitSuccess = s

-- | Handler catching only IOExceptions and setting an error in the AppState
handleIOException :: AppState -> IOException -> IO AppState
handleIOException s' ex = pure $ s' & setError (ProcessError (show ex))

-- | Try running a process given by the `FilePath`
tryRunProcess :: ProcessConfig stdout stderr stdin -> IO (Either IOException ExitCode)
tryRunProcess = try . runProcess

setError :: Error -> AppState -> AppState
setError = set asError . Just
