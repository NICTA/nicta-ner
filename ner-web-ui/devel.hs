{-# LANGUAGE PackageImports #-}

{-
NICTA t3as NER Web UI
Copyright 2014 NICTA

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/gpl-3.0.html>.
-}

import "ner-web-ui" Application (getApplicationDev)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, setPort)
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    forkIO $ runSettings (setPort port defaultSettings) app
    loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "yesod-devel/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
