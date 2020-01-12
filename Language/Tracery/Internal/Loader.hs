{-# LANGUAGE NoImplicitPrelude #-}
module Language.Tracery.Internal.Loader(loadGrammar, parseGrammar) where

import Prelude
import qualified Data.HashMap.Strict as HMS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as Types
import qualified Data.Vector as V
import qualified Data.Text as T

import Language.Tracery.Internal.Grammar
import Language.Tracery.Internal.Sentence

type Error = String

loadGrammar :: FilePath -> IO Grammar
loadGrammar fp = do
    content <- LBS.readFile fp
    case parseGrammar content of
        Right grammar -> pure grammar
        Left err -> error err

parseGrammar :: LBS.ByteString -> Either Error Grammar
parseGrammar = objToGrammar . A.eitherDecode

objToGrammar :: Either Error Types.Value -> Either Error Grammar
objToGrammar (Right (Types.Object obj)) = HMS.foldlWithKey' addToGrammar (Right HMS.empty) obj
objToGrammar (Right _) = Left "invalid grammar file structure (not an object)"
objToGrammar (Left err) = Left err

addToGrammar :: Either Error Grammar -> T.Text -> Types.Value -> Either Error Grammar
addToGrammar (Left err) _ _ = Left err
addToGrammar (Right acc) k (Types.Array rawSentences) =
    case V.foldl (parseSentences key) (Right []) (withIndex rawSentences) of
      (Right sentences) -> Right $ HMS.insert key sentences acc
      (Left err) -> Left err
  where withIndex v = V.zip (V.fromList [1..length v]) v
        key = fromText k
addToGrammar _ key _ = Left $ T.unpack key ++ ": invalid sentence structure"

parseSentences :: Symbol -> Either Error [Sentence] -> (Int, Types.Value) -> Either Error [Sentence]
parseSentences key (Left err) (_, _) = Left $ asString key ++ ": " ++ err
parseSentences key (Right acc) (n, Types.String rawSentence) =
    case parseSentence (fromText (T.concat [asText key, T.pack ":", T.pack $ show n])) rawSentence of
        (Right sen) -> Right (sen : acc)
        (Left err) -> Left $ show err
parseSentences key _ _ = Left $ "invalid sentence type for " ++ asString key