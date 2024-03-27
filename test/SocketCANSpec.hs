module SocketCANSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Samples

import qualified Network.SocketCAN.Translate

spec :: Spec
spec = do
  describe "SocketCAN" $ do
    it "roundtrips" $ do
      mapM_
        (\x ->
          ( Network.SocketCAN.Translate.fromSocketCANFrame
          . Network.SocketCAN.Translate.toSocketCANFrame
          ) x
          `shouldBe` x
        ) samples
