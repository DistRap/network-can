module SLCANSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Samples

import Network.SLCAN.Types (SLCANMessage(..))
import qualified Network.SLCAN.Builder
import qualified Network.SLCAN.Parser

spec :: Spec
spec = do
  describe "SLCAN" $ do
    it "roundtrips samples" $ do
      mapM_
        (\x ->
          (   Network.SLCAN.Parser.parseSLCANMessage
          <$> Network.SLCAN.Builder.buildSLCANMessage
          ) (SLCANMessage_Data x)
          `shouldBe` pure (SLCANMessage_Data x)
        ) samples

    prop "roundtrips" $ \x ->
      Network.SLCAN.Parser.parseSLCANMessage
        (Network.SLCAN.Builder.buildSLCANMessage x)
      `shouldBe`
        pure x
