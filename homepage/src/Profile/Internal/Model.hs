{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Profile.Internal.Model where

import           Database.Persist.TH
import           Data.Aeson
import           GHC.Generics

share
  [mkPersist sqlSettings, mkMigrate "migrateProfile"]
  [persistLowerCase|
Profile
    name String
    picture String
    tagline String
    deriving Show
    deriving Eq
    deriving Generic
|]

instance ToJSON Profile

share
  [mkPersist sqlSettings, mkMigrate "migrateEducation"]
  [persistLowerCase|
Education
    institution String
    info String
    from String
    until String
    description String
    picture String
    active Bool
    deriving Show
    deriving Eq
    deriving Generic
|]

instance ToJSON Education

share
  [mkPersist sqlSettings, mkMigrate "migrateProject"]
  [persistLowerCase|
Project
    name String
    link String
    logo String
    description String
    deriving Show
    deriving Eq
    deriving Generic
|]

instance ToJSON Project

share
  [mkPersist sqlSettings, mkMigrate "migrateAchievement"]
  [persistLowerCase|
Achievement
    name String
    date String
    description String
    picture String
    deriving Show
    deriving Eq
    deriving Generic
|]

instance ToJSON Achievement

share
  [mkPersist sqlSettings, mkMigrate "migrateExperience"]
  [persistLowerCase|
Experience
    name String
    info String
    from String
    until String
    description String
    picture String
    deriving Show
    deriving Eq
    deriving Generic
|]

instance ToJSON Experience

data CompleteProfile = CompleteProfile
  { profile :: Maybe Profile
  , educations :: [Education]
  , projects :: [Project]
  , experiences :: [Experience]
  , achievements :: [Achievement]
  }
  deriving (Show, Eq, Generic)

instance ToJSON CompleteProfile