{-# LANGUAGE FlexibleInstances #-}

module Meme.Internal.GetterSpec where

import           Test.Hspec
import           Meme.Internal.Getter
import           Utils.Directory
import           Utils.Config
import           Control.Monad.State

baseDirectory :: FilePath
baseDirectory = "meme/dir"

directoryContent :: [FilePath]
directoryContent = ["a", "b", "c"]

file :: FilePath
file = head directoryContent

instance ConfigHandler (StateT () IO) where
    getMemeDir = return baseDirectory

instance DirIO (StateT () IO) where
    listDir path = case path of
        baseDirectory -> return directoryContent

spec :: Spec
spec = do
    describe "getMemes" $ do
        it "Returns file list from config dir" $ do
            evalStateT getMemes () `shouldReturn` directoryContent

    describe "getMeme" $ do
        it "Returns file path" $ do
            evalStateT (getMeme file) ()
                `shouldReturn` (baseDirectory ++ "/" ++ file)
