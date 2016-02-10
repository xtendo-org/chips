module Args
    ( Cmd(..)
    , getCmd
    ) where

import System.Exit (exitSuccess)
import System.Console.CmdArgs.Explicit

data Cmd
    = CmdInit
    | CmdInstall String
    | CmdRemove String
    | CmdUpgrade
    | CmdClean

arguments :: Mode (Maybe Cmd)
arguments = modes "fplug" Nothing
    "A plugin manager for the fish shell"
    [ m "init" CmdInit "get fplug ready for use"
    , mp "install" CmdInstall "install a plugin"
    , mp "remove" CmdRemove "remove a plugin"
    , m "upgrade" CmdUpgrade "upgrade installed plugins"
    , m "clean" CmdClean "clean unused plugins"
    ]
  where
    m name cmd desc = mode name (Just cmd) desc
        (flagArg (\_ c -> Right c) "") []
    mp name cmd desc = mode name (Just $ cmd "") desc
        (flagArg (\a _ -> Right $ Just $ cmd a) "PLUGIN_NAME") []

getCmd :: IO Cmd
getCmd = processArgs arguments >>= maybe helpDie return
  where
    helpDie = print (helpText [] HelpFormatAll arguments) *> exitSuccess
