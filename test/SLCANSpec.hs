module SLCANSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Samples

import qualified Network.SLCAN.Builder
import qualified Network.SLCAN.Parser

spec :: Spec
spec = do
  describe "SLCAN" $ do
    it "roundtrips" $ do
      mapM_
        (\x ->
          (   Network.SLCAN.Parser.parseSLCANMessage
          <$> Network.SLCAN.Builder.buildSLCANMessage
          ) x
          `shouldBe` pure x
        ) samples
