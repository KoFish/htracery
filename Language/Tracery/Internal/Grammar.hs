{-# LANGUAGE NoImplicitPrelude #-}
module Language.Tracery.Internal.Grammar where

import Prelude
import Data.Hashable
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

type Grammar = HM.HashMap Symbol [Sentence]
newtype Symbol = Symbol T.Text
newtype Sentence = Sentence [Component]
data Component = Act Action
               | Lit Literal
data Action = Expand SymbolRef
            | Store Symbol Sentence
newtype Literal = Literal T.Text
data SymbolRef = Sym Symbol Modifiers
type Modifiers = S.Set Modifier
data Modifier = Capitalize
              | Pluralize
              | AAn
    deriving( Show, Eq, Ord )

(|/|) :: Grammar -> Symbol -> Maybe [Sentence]
g |/| sym = HM.lookup sym g

specialSymbols :: String
specialSymbols = ['\\', '#', '[', ']', '=']

newGrammar :: Symbol -> [Sentence] -> Grammar
newGrammar = HM.singleton

setGrammar :: Symbol -> [Sentence] -> Grammar -> Grammar
setGrammar = HM.insert

emptySentence :: Sentence
emptySentence = Sentence []

showSentence :: Sentence -> String
showSentence (Sentence s) = (L.intercalate ", " . map show) s

showShortSentence :: Sentence -> String
showShortSentence (Sentence s) = (L.intercalate ", " . map showLessComponent) s

showLessComponent :: Component -> String
-- Hide nested stores because these mess up the quickcheck tests by being potentially infinite.
showLessComponent (Act (Store sym _)) = show "{Action {Store `" ++ asString sym ++ "` = ...}}"
showLessComponent c = show c

class AComponent a where
    fromString :: String -> a
    fromText :: T.Text -> a
    fromText s = fromString (T.unpack s)
    asString :: a -> String
    asText :: a -> T.Text
    asText s = T.pack (asString s)

quoteString :: String -> String
quoteString (c : rest) | c `elem` specialSymbols = '\\' : c : quoteString rest
                       | otherwise               = c : quoteString rest
quoteString [] = []

unquoteString :: String -> String
unquoteString ('\\' : c : rest) | c `elem` specialSymbols = c : unquoteString rest
unquoteString (c : rest) = c : unquoteString rest
unquoteString [] = []

instance AComponent Symbol where
    asString (Symbol s) = T.unpack s
    fromString s = Symbol $ T.pack s

instance AComponent Literal where
    asString (Literal l) = (quoteString . T.unpack) l
    fromString l = Literal $ (T.pack . unquoteString) l

instance Show Sentence where
    show = showShortSentence

instance Hashable Symbol where
    hashWithSalt salt (Symbol s) = hashWithSalt salt s

instance Show Symbol where
    show (Symbol sym) = T.unpack sym

instance Eq Symbol where
    (Symbol a) == (Symbol b) = a == b

instance Show Literal where
    show (Literal lit) = T.unpack lit

instance Show SymbolRef where
    show (Sym s ms) = "{SymRef `" ++ show s ++ "` " ++ show (S.toList ms) ++ "}"

instance Show Component where
    show (Lit t) = "{Lit \"" ++ show t ++ "\"}"
    show (Act a) = "{Action " ++ show a ++ "}"

instance Show Action where
    show (Expand symref) = "{Expand " ++ show symref ++ "}"
    show (Store sym sen) = "{Store `" ++ show sym ++ "` = " ++ showShortSentence sen ++ "}"