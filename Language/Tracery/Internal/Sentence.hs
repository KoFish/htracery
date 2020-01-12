{-# LANGUAGE NoImplicitPrelude #-}
module Language.Tracery.Internal.Sentence where

import           Prelude
import           Language.Tracery.Internal.Grammar as G
import           Language.Tracery.Internal.Parsers
import           Text.Parsec
import qualified Data.Set                      as S
import qualified Data.Text                     as T


parseSentence :: G.Symbol -> T.Text -> Either ParseError G.Sentence
parseSentence _ raw | T.null raw = Right emptySentence
parseSentence inputName raw = parse pWholeString (show inputName) (T.unpack raw)

-- SERIALIZE SENTENCE

serializeSentence :: Sentence -> T.Text
serializeSentence (Sentence (Lit l : rest)) = T.concat [asText l, serializeSentence (Sentence rest)]
serializeSentence (Sentence list@(Act _ : _)) = case compressActions list of
    (actions, rest) -> T.concat [T.singleton '#', T.concat (map serializeAction actions), T.singleton '#', serializeSentence (Sentence rest)]
serializeSentence (Sentence []) = T.empty

compressActions :: [Component] -> ([Action], [Component])
compressActions (Act a@(Expand _) : rest@(Act (Store _ _) : _)) =
    case compressActions rest of
        (acts, others) -> (a : acts, others)
compressActions (Act a@(Store _ _) : rest@(Act _ : _)) =
    case compressActions rest of
        (acts, others) -> (a : acts, others)
compressActions (Act a@(Expand _) : rest@(Act (Expand _) : _)) =
    ([a], rest)
compressActions (Act a : rest) =
    ([a], rest)
compressActions rest =
    ([], rest)

serializeAction :: Action -> T.Text
serializeAction (Expand (Sym (Symbol s) ms)) = T.concat [s, serializeMods (S.toList ms)]
serializeAction (Store (Symbol s) sen) = T.concat [T.singleton '[', s, T.singleton ':', serializeSentence sen , T.singleton ']']

serializeMods :: [Modifier] -> T.Text
serializeMods (Capitalize : rest) = T.concat [T.pack ".c", serializeMods rest]
serializeMods (Pluralize : rest) = T.concat [T.pack ".p", serializeMods rest]
serializeMods (AAn : rest) = T.concat [T.pack ".a", serializeMods rest]
serializeMods [] = T.empty
