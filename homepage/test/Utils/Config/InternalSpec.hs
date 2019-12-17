{-# LANGUAGE FlexibleInstances #-}

module Utils.Config.InternalSpec where

import           Control.Arrow
import           Control.Exception
import           Control.Monad.State
import           Data.Functor.Identity
import qualified Fixtures              as F
import           System.IO.Unsafe
import           Test.Hspec
import           Utils.Config.Internal

data MockConfig = MockConfig Config
                | MockConfigError String

instance ConfigIO (StateT MockConfig IO) where
    decodeFileEither _ = do
        mockConfig <- get
        return $ case mockConfig of
            MockConfig      x -> Right x
            MockConfigError x -> Left $ error x

spec :: Spec
spec = do
    describe "getConfig" $ do
        it "Handle correct yaml" $ do
            evalStateT getConfig (MockConfig F.config)
                `shouldReturn` F.config
        it "Handle error" $ do
            let config =
                    evalStateT getConfig (MockConfigError "file not found")
            (config `shouldNotReturn` F.config)
                `shouldThrow` errorCall "file not found"

    describe "getMemeDir" $ do
        it "Returns the correct value" $ do
            evalStateT getMemeDir (MockConfig F.config)
                `shouldReturn` (meme >>> memeDir) F.config

    describe "getDBConfig" $ do
        it "Returns the correct value" $ do
            evalStateT getDBConfig (MockConfig F.config)
                `shouldReturn` db F.config
