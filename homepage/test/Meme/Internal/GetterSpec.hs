{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Meme.Internal.GetterSpec where

import           Control.Monad.State
import           Debug.Trace
import qualified Fixtures             as F
import           Meme.Internal.Getter
import           Test.Hspec
import           Utils.Config
import           Utils.Directory
import           Utils.Env

instance ConfigHandler (StateT () IO) where
    getMemeDir = return F.directory
    getConfig  = undefined

instance DirIO (StateT () IO) where
    listDir path =
        return $ if path == F.directory
            then F.directoryContent
            else undefined

spec :: Spec
spec = do
    describe "getMemes" $ do
        it "Returns file list from config dir" $ do
            evalStateT (getMemes F.env) () `shouldReturn` F.directoryContent

    describe "getMeme" $ do
        it "Returns file path" $ do
            getMeme F.env F.file
                `shouldBe` (F.directory ++ "/" ++ F.file)
