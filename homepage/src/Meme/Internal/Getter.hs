module Meme.Internal.Getter where

import qualified Utils.Config                  as Config
import           Utils.Directory
import           Utils.Env as Env
import           Control.Arrow

getMemes :: DirIO m => Config.ConfigHandler m => Env.Env -> m [FilePath]
getMemes env = listDir memeDir 
    where 
        memeDir = Config.memeDir
                $ Config.meme 
                $ Env.config env

getMeme :: Env.Env -> FilePath -> FilePath
getMeme env filename = memeDir ++ "/" ++ filename 
    where 
        memeDir = Config.memeDir
                $ Config.meme 
                $ Env.config env
