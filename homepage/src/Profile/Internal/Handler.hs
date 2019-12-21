module Profile.Internal.Handler where

import           Control.Monad.IO.Class
import qualified Profile.Internal.Model        as Model
import           Database.Persist
import           Database.Persist.Sql
import           Utils.DB                      as DB

class MonadIO m => ProfileHandler m where
    getProfile :: ConnectionPool -> m (Maybe Model.CompleteProfile)

instance ProfileHandler IO where
    getProfile pool = flip runSqlPool pool $ do
        profile <- selectList [] [LimitTo 1]
        educations <- selectList [] []
        projects <- selectList [] []
        return $ Just Model.CompleteProfile 
            { Model.profile = DB.entityHead profile
            , Model.educations = map entityVal educations
            , Model.projects = map entityVal projects
            }
