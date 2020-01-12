module Language.Tracery.Internal.Parsers where

import Language.Tracery.Internal.Grammar as G
import qualified Data.Set as S
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String

pWholeString :: Parser G.Sentence
pWholeString = do
    sen <- pSentence;
    _ <- eof;
    return sen

pSentence :: Parser G.Sentence
pSentence = Sentence <$> pComponents

pSymbol :: Parser G.Symbol
pSymbol = fromString <$> many1 alphaNum

pComponents :: Parser [G.Component]
pComponents = do
    comps <- (map Lit <$> pLiterals) <|> (map Act <$> pActionList)
    rest <- option [] pComponents
    return $ comps ++ rest

pEscapedString :: Parser String
pEscapedString = do
    piece <- many $ noneOf specialSymbols
    rest <- option "" pEscapedString'
    let str = piece ++ rest
    case str of
        [] -> unexpected "empty literal"
        _ -> return str

pEscapedString' :: Parser String
pEscapedString' = do
    _ <- char '\\'
    c <- anyChar
    rest <- option "" pEscapedString
    return $ '\\' : c : rest

pLiterals :: Parser [G.Literal]
pLiterals = do
    l <- pLiteral
    return [l]

pLiteral :: Parser G.Literal
pLiteral = fromString <$> pEscapedString

pActionList :: Parser [G.Action]
pActionList = do
    _ <- char '#'
    actions <- many1 pAction
    _ <- char '#'
    return actions

pAction :: Parser G.Action
pAction = pStoreAction <|> pExpandAction

pStoreAction :: Parser G.Action
pStoreAction = do
    _ <- char '['
    sym <- pSymbol
    _ <- char ':'
    sen <- option emptySentence pSentence
    _ <- char ']'
    return $ G.Store sym sen

pExpandAction :: Parser G.Action
pExpandAction = Expand <$> pSymRef

pSymRef :: Parser G.SymbolRef
pSymRef = do
    sym <- pSymbol
    mods <- many pMod
    return $ G.Sym sym (S.fromList mods)

pMod :: Parser G.Modifier
pMod = do
    _ <- char '.'
    (Symbol modifier) <- pSymbol
    case T.unpack modifier of
        "c" -> return Capitalize
        "p" -> return Pluralize
        "a" -> return AAn
        _ -> unexpected "unknown modifier"

