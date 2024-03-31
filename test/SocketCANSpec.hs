module SocketCANSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Samples

import qualified Network.SocketCAN.Translate

spec :: Spec
spec = do
  describe "SocketCAN" $ do
    it "roundtrips samples" $ do
      mapM_
        (\x ->
          ( Network.SocketCAN.Translate.fromSocketCANFrame
          . Network.SocketCAN.Translate.toSocketCANFrame
          ) x
          `shouldBe` x
        ) samples

    prop "roundtrips" $ \x ->
      Network.SocketCAN.Translate.fromSocketCANFrame
        (Network.SocketCAN.Translate.toSocketCANFrame x)
      `shouldBe`
        x
