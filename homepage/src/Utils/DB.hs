module Utils.DB where

import           Database.Persist

entityHead :: [Entity a] -> Maybe a
entityHead profile =
    if null profile then Nothing else Just $ entityVal $ head profile
