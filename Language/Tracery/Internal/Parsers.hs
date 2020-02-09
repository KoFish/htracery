module Language.Tracery.Internal.Parsers where

import Language.Tracery.Internal.Grammar as G
import Language.Tracery.Internal.Modifiers as M
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
pSymbol = Symbol <$> many1 alphaNum

pComponents :: Parser [G.Component]
pComponents = do
    comps <- ((: []) . Literal <$> pLiteral) <|> (map Action <$> pActionList)
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

pLiteral :: Parser G.LiteralComp
pLiteral = fromString <$> pEscapedString

pActionList :: Parser [G.ActionComp]
pActionList = do
    _ <- char '#'
    actions <- many1 pAction
    _ <- char '#'
    return actions

pAction :: Parser G.ActionComp
pAction = pStoreAction <|> pExpandAction

pStoreAction :: Parser G.ActionComp
pStoreAction = do
    _ <- char '['
    sym <- pSymbol
    _ <- char ':'
    sen <- option emptySentence pSentence
    _ <- char ']'
    return $ G.Store sym sen

pExpandAction :: Parser G.ActionComp
pExpandAction = Expand <$> pSymRef

pSymRef :: Parser G.SymbolRef
pSymRef = do
    sym <- pSymbol
    mods <- many pMod
    return $ G.Sym sym mods

pMod :: Parser M.ModifierName
pMod = do
    _ <- char '.'
    (Symbol modifier) <- pSymbol
    return $ Mod modifier

