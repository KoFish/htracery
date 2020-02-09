{-# LANGUAGE NoImplicitPrelude #-}
module Language.Tracery.Internal.Grammar where

import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T
import           Prelude
import Language.Tracery.Internal.Modifiers

-- |Describes the possible traces.
--  A map of 'Symbols' to a list of 'Sentences'.
type Grammar = HM.HashMap Symbol [Sentence]
newtype Symbol = Symbol String
newtype Sentence = Sentence [Component]
  deriving( Eq )
data Component = Action ActionComp
               | Literal LiteralComp
  deriving( Eq )
data ActionComp = Expand SymbolRef
                | Store Symbol Sentence
  deriving( Eq )
newtype LiteralComp = Lit T.Text
  deriving( Eq )
data SymbolRef = Sym Symbol [ModifierName]
  deriving( Eq )

-- |Fetch the given 'Symbol' from the 'Grammar'.
lookup :: Symbol -> Grammar -> Maybe [Sentence]
lookup sym gram = HM.lookup sym gram

specialSymbols :: String
specialSymbols = ['\\', '#', '[', ']', '=']

newGrammar :: Symbol -> [Sentence] -> Grammar
newGrammar = HM.singleton

addGrammar :: Symbol -> [Sentence] -> Grammar -> Grammar
addGrammar = HM.insert

emptySentence :: Sentence
emptySentence = Sentence []

showSentence :: Sentence -> String
showSentence (Sentence s) = (L.intercalate ", " . map show) s

showShortSentence :: Sentence -> String
showShortSentence (Sentence s) = (L.intercalate ", " . map showLessComponent) s

showLessComponent :: Component -> String
-- Hide nested stores because these mess up the quickcheck tests by being potentially infinite.
showLessComponent (Action (Store sym _)) = show "{Action {Store `" ++ show sym ++ "` = ...}}"
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

instance AComponent LiteralComp where
    asString (Lit l) = (quoteString . T.unpack) l
    fromString l = Lit $ (T.pack . unquoteString) l

instance Show Sentence where
    show = showShortSentence

instance Hashable Symbol where
    hashWithSalt salt (Symbol s) = hashWithSalt salt s

instance Show Symbol where
    show (Symbol sym) = "`" ++ sym ++ "`"

instance Eq Symbol where
    (Symbol a) == (Symbol b) = a == b

instance Show SymbolRef where
    show (Sym s ms) = "{SymRef `" ++ show s ++ "` " ++ (L.intercalate "." . map show) ms ++ "}"

instance Show LiteralComp where
    show (Lit lit) = T.unpack lit

instance Show ActionComp where
    show (Expand symref) = "{Expand " ++ show symref ++ "}"
    show (Store sym sen) = "{Store `" ++ show sym ++ "` = " ++ showShortSentence sen ++ "}"

instance Show Component where
    show (Literal t) = "{Lit \"" ++ show t ++ "\"}"
    show (Action a) = "{Action " ++ show a ++ "}"

