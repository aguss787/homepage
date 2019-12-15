module Meme.Internal.Getter where

import qualified Utils.Config                  as Config
import           Utils.Directory

getMemes :: DirIO m => Config.ConfigHandler m => m [FilePath]
getMemes = listDir =<< Config.getMemeDir

getMeme :: Config.ConfigHandler m => FilePath -> m FilePath
getMeme filename = flip (++) ("/" ++ filename) <$> Config.getMemeDir

