module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Pruebas casino Royale" $ do
    it "mayorSegun absoluto entre 2 y -3 es -3" $ do
      mayorSegun abs 2 (-3) `shouldBe` (-3)
    it "maximoSegun absoluto entre [2 ,-3] es -3" $ do
      maximoSegun abs [2, (-3)] `shouldBe` (-3)