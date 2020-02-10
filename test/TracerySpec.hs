{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module TracerySpec (spec) where

import Prelude
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck()
import Arbitraries()
import Language.Tracery.Internal.Loader
import Language.Tracery.Internal.Grammar as Grammar
import Language.Tracery.Internal.Memory
import Language.Tracery.Internal.Modifiers
import Language.Tracery.Internal.Eval
import Text.RawString.QQ

import System.Random
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as Char8

simpleGrammar1 :: LBS.ByteString
simpleGrammar1 = Char8.pack [r|{
  "origin": ["foobar"]
}|]

simpleGrammar2 :: LBS.ByteString
simpleGrammar2 = Char8.pack [r|{
    "origin": ["foo#foo#"],
    "foo": ["bar"]
  }
|]

spec :: Spec
spec =
  describe "Sentence.hs" $ do
    it "parse simple grammar" $
      let Right gram = parseGrammar simpleGrammar1
          Just origin = Grammar.lookup (Symbol "origin") gram
          expectedSentence = Sentence [Literal (Lit $ T.pack "foobar")]
      in origin `shouldBe` [expectedSentence]
    it "trace simple grammar" $
      let Right gram = parseGrammar simpleGrammar1
          memory = initMemory gram englishModifiers g
          trace = T.unpack $ runTracery fromOrigin memory
      in trace `shouldBe` "foobar"
    it "trace simple grammar 2" $
      let Right gram = parseGrammar simpleGrammar2
          memory = initMemory gram englishModifiers g
          trace = T.unpack $ runTracery fromOrigin memory
      in trace `shouldBe` "foobar"
    it "trace grammar with unidentified ref" $
      let Right gram = parseGrammar $ Char8.pack [r|{"origin": ["#test#"]}|]
          memory = initMemory gram englishModifiers g
          trace = T.unpack $ runTracery fromOrigin memory
      in trace `shouldBe` ""
    it "trace grammar with defined default modifier" $
      let Right gram = parseGrammar $ Char8.pack [r|{"origin": ["#foo.c#"], "foo": ["test"]}|]
          memory = initMemory gram englishModifiers g
          trace = T.unpack $ runTracery fromOrigin memory
      in trace `shouldBe` "Test"
    it "trace grammer with multiple modifiers" $
      let Right gram = parseGrammar $ Char8.pack [r|{"origin": ["#foo.c.a# #foo.ed.a.t# #foo#"], "foo": ["test"]}|]
          memory = initMemory gram englishModifiers g
          trace = T.unpack $ runTracery fromOrigin memory
      in trace `shouldBe` "a Test A Tested test"
    it "trace with memory" $ property prop_memory_test
  where g = mkStdGen 1
        fromOrigin = tracery $ Symbol "origin"

memoryGrammar :: LBS.ByteString
memoryGrammar = Char8.pack [r|
  {
    "either": ["a", "b", "c"],
    "orTheOther": ["#one#"],
    "start":["#[one:#either#]##one##orTheOther#"]
  }
|]

prop_memory_test :: Int -> Property
prop_memory_test seed =
  case parseGrammar memoryGrammar >>=
                   \gram ->
                     let memory = initMemory gram englishModifiers (mkStdGen (seed + 1))
                         trace = runTracery (tracery $ Symbol "start") memory
                     in return $ T.unpack trace
  of
    Right [a, b] -> a === b
    Right x -> counterexample ("expected string of two equal characters: " ++ show x) False
    Left err -> counterexample ("memory grammar could not be parsed: " ++ err) False
