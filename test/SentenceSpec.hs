{-# LANGUAGE NoImplicitPrelude #-}
module SentenceSpec (spec) where

import Prelude
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck()
import Arbitraries()
import Language.Tracery.Internal.Grammar
import Language.Tracery.Internal.Sentence

import Data.List
import qualified Data.Text as T

makeCanonical :: String -> String
makeCanonical orgSen = case parseSentence (fromString "fooBar") (T.pack orgSen) of
    Right sen -> makeCanonical' sen
    Left err -> show err

makeCanonical' :: Sentence -> String
makeCanonical' orgSen = T.unpack $ serializeSentence orgSen

spec :: Spec
spec =
  describe "Sentence.hs" $ do
    it "compresses single action" $
      makeCanonical "#foo#bar" `shouldBe` "#foo#bar"
    it "compresses actions" $
      makeCanonical "#[foo:bar]##expandThis#" `shouldBe` "#[foo:bar]expandThis#"
    it "doesn't compress consequtive expands" $
      makeCanonical "#foo##bar#" `shouldBe` "#foo##bar#"
    it "compress all actions" $ property prop_compress_actions
    it "serialize sentence" $ property prop_serialize_sentence

prop_compress_actions :: [Component] -> Bool
prop_compress_actions [] = True
prop_compress_actions cs =
  case (uncons (reverse compressed), uncons rest) of
    (Nothing, Just (Act _, _)) -> False
    (Just (_, _), Nothing) -> True
    (Just (Expand _, _), Just (Act (Expand _), _)) -> True
    (Just _, Just _) -> True
    (Nothing, Just _) -> True
    (_, _) -> False
  where (compressed, rest) = compressActions cs

prop_serialize_sentence :: Sentence -> Bool
prop_serialize_sentence sen =
  makeCanonical' sen == makeCanonical (makeCanonical' sen)