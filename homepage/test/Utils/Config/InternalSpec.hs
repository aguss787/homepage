{-# LANGUAGE FlexibleInstances #-}

module Utils.Config.InternalSpec where

import           Test.Hspec
import           Utils.Config.Internal
import           Control.Monad.State
import           Control.Exception
import           Control.Arrow
import           Data.Functor.Identity
import           System.IO.Unsafe

data MockConfig = MockConfig Config
                | MockConfigError String

instance ConfigIO (StateT MockConfig IO) where
    decodeFileEither _ = do
        mockConfig <- get
        return $ case mockConfig of
            MockConfig      x -> Right x
            MockConfigError x -> Left $ error x

sampleConfig :: Config
sampleConfig = Config (MemeConfig "meme/dir")

spec :: Spec
spec = do
    describe "getConfig" $ do
        it "Handle correct yaml" $ do
            evalStateT getConfig (MockConfig sampleConfig)
                `shouldReturn` sampleConfig
        it "Handle error" $ do
            let config =
                    evalStateT getConfig (MockConfigError "file not found")
            (config `shouldNotReturn` sampleConfig)
                `shouldThrow` errorCall "file not found"

    describe "getMemeDir" $ do
        it "Returns the correct value" $ do
            evalStateT getMemeDir (MockConfig sampleConfig)
                `shouldReturn` (meme >>> memeDir) sampleConfig
