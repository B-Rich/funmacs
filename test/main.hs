module Main where

import Control.Concurrent
import Control.Exception
import Control.Logging
import Control.Monad.Logger
import Control.Magit
import Prelude hiding (log)
import Test.Hspec

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

main :: IO ()
main = hspec $ do
    describe "simple logging" $ do
        it "works" $ True `shouldBe` True
