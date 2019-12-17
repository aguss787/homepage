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
  [mkPersist sqlSettings]
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
