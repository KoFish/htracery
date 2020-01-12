module Language.Tracery.Internal.Eval
    ( tracery
    , runTracery
    ) where

import System.Random
import Control.Monad.State
import Language.Tracery.Internal.Grammar
import qualified Data.Text as T

type Tracery a = State (Grammar, StdGen) a

tracery :: Symbol -> Tracery [T.Text]
tracery startSym = do
    grammar <- gets fst
    case grammar |/| startSym of
        Just sentences -> do
            (Sentence sentence) <- pickOne sentences
            evalSentence sentence
        Nothing -> return []

evalSentence :: [Component] -> Tracery [T.Text]
evalSentence [] = return []
evalSentence (Lit (Literal l) : rest) = do
    cont <- evalSentence rest
    return $ l : cont
evalSentence (Act (Expand symref) : rest) = do
    l <- tracery symbol
    cont <- evalSentence rest
    return $ l ++ cont
  where (Sym symbol _) = symref
evalSentence (Act (Store symbol sentence) : rest) = do
    _ <- modify (\(grammar, g) -> (setGrammar symbol [sentence] grammar, g))
    evalSentence rest

runTracery :: Tracery [T.Text] -> StdGen -> Grammar -> T.Text
runTracery st g grammar = do
    let (result, _) = runState st (grammar, g)
    T.concat result

pickOne :: [a] -> Tracery a
pickOne list = do
    (grammar, g) <- get
    let (e, g') = randomR (0, length list - 1) g
    _ <- put (grammar, g')
    return $ list !! e