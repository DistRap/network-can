module CANSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Samples

import qualified Network.CAN

spec :: Spec
spec = do
  describe "CAN" $ do
    it "pretty prints samples" $
      map Network.CAN.prettyCANMessage samples
      `shouldBe`
      [ "     000   [0] "
      , "     FFF   [0] "
      , "     123   [2]  DE AD"
      , "     123   [0]  remote request"
      , "00000000   [0] "
      , "00123456   [1]  EE"
      , "00123456   [0]  remote request"
      ]
