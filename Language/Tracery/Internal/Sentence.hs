{-# LANGUAGE NoImplicitPrelude #-}
module Language.Tracery.Internal.Sentence where

import           Prelude
import           Language.Tracery.Internal.Grammar as G
import           Language.Tracery.Internal.Modifiers as M
import           Language.Tracery.Internal.Parsers
import           Text.Parsec
import qualified Data.Text                     as T


parseSentence :: G.Symbol -> T.Text -> Either ParseError G.Sentence
parseSentence _ raw | T.null raw = Right emptySentence
parseSentence inputName raw = parse pWholeString (show inputName) (T.unpack raw)

-- SERIALIZE SENTENCE

serializeSentence :: Sentence -> T.Text
serializeSentence (Sentence (Literal l : rest)) = T.concat [asText l, serializeSentence (Sentence rest)]
serializeSentence (Sentence list@(Action _ : _)) = case compressActions list of
    (actions, rest) -> T.concat [T.singleton '#', T.concat (map serializeAction actions), T.singleton '#', serializeSentence (Sentence rest)]
serializeSentence (Sentence []) = T.empty

compressActions :: [Component] -> ([ActionComp], [Component])
compressActions (Action a@(Expand _) : rest@(Action (Store _ _) : _)) =
    case compressActions rest of
        (acts, others) -> (a : acts, others)
compressActions (Action a@(Store _ _) : rest@(Action _ : _)) =
    case compressActions rest of
        (acts, others) -> (a : acts, others)
compressActions (Action a@(Expand _) : rest@(Action (Expand _) : _)) =
    ([a], rest)
compressActions (Action a : rest) =
    ([a], rest)
compressActions rest =
    ([], rest)

serializeAction :: ActionComp -> T.Text
serializeAction (Expand (Sym (Symbol s) ms)) = T.concat (T.pack s : M.serializeMods ms)
serializeAction (Store (Symbol s) sen) = T.concat [T.singleton '[', T.pack s, T.singleton ':', serializeSentence sen , T.singleton ']']
