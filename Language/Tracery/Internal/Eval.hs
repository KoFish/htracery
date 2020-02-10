module Language.Tracery.Internal.Eval
    ( tracery
    , runTracery
    ) where

import           Control.Monad.State
import qualified Data.Text as T
import           Language.Tracery.Internal.Grammar as Grammar
import           Language.Tracery.Internal.Modifiers
import           Language.Tracery.Internal.Memory
import           System.Random

type Tracery a = State Memory a

-- |Creates a 'Tracery' instance that generates a trace using the given
--  'Symbol' as origin.
tracery :: Symbol -> Tracery T.Text
tracery startSym = do
    gram <- gets grammar
    case Grammar.lookup startSym gram of
        Just sentences -> do
            (Sentence sentence) <- pickOne sentences
            evalSentence sentence
        Nothing -> return T.empty

-- |Create a 'Tracery' instance that produces the trace of a specific
--  component in the used grammar.
evalComponent :: Component -> Tracery T.Text
evalComponent (Literal (Lit l)) = return l
evalComponent (Action (Expand symref)) = do
    expanded <- tracery symbol
    applyModifiers mods expanded
  where (Sym symbol mods) = symref
evalComponent (Action (Store symbol (Sentence sentence))) = do
    expanded <- evalSentence sentence
    _ <- modify (\mem -> withGrammar (addGrammar symbol (asSentence expanded) (grammar mem)) mem)
    return T.empty
  where asSentence expanded = [Sentence [Literal (Lit expanded)]]

-- |Create a 'Tracery' instance that applies 'Modifiers' to the given text.
applyModifiers :: [ModifierName] -> T.Text -> Tracery T.Text
applyModifiers (modifier : rest) t = do
    mods <- gets modifiers
    applyModifiers rest (applyModifier mods modifier t)
applyModifiers [] t = return t

-- |Create a 'Tracery' instance that produces a trace of a list of components.
evalSentence :: [Component] -> Tracery T.Text
evalSentence [] = return T.empty
evalSentence (c : rest) = do
    expanded <- evalComponent c
    cont <- evalSentence rest
    return $ T.concat [expanded, cont]

-- |Run a 'Tracery' instance to produce the actual trace with a specific
--  randomiser and 'Grammar'. This uses the 'englishModifiers'.
runTracery :: Tracery T.Text -> Memory -> T.Text
runTracery st mem = do
    let (result, _) = runState st mem
    result

-- UTILITY FUNCTIONS:

pickOne :: [a] -> Tracery a
pickOne list = do
    g <- gets gen
    let (e, g') = oneOf list g
    _ <- modify (\(Memory gram mods _) -> Memory gram mods g')
    return e

oneOf :: [a] -> StdGen -> (a, StdGen)
oneOf list g = (element, g')
  where (index, g') = randomR (0, length list - 1) g
        element = list !! index