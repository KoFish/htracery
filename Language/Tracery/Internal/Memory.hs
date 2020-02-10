module Language.Tracery.Internal.Memory where

import Language.Tracery.Internal.Grammar
import Language.Tracery.Internal.Modifiers
import System.Random

-- |Internal state of a Tracery instance.
data Memory = Memory Grammar Modifiers StdGen

-- |Create a new 'Memory' to use for evaluation.
initMemory :: Grammar -> Modifiers -> StdGen -> Memory
initMemory = Memory

-- |Get the 'Grammar' part of the 'Memory'.
grammar :: Memory -> Grammar
grammar (Memory gram _ _) = gram

-- |Get the 'Modifiers' part of the 'Memory'.
modifiers :: Memory -> Modifiers
modifiers (Memory _ m _) = m

-- |Get the 'StdGen' part of 'Memory'.
gen :: Memory -> StdGen
gen (Memory _ _ g) = g

-- |Return a new 'Memory' with an updated randomizer.
withGen :: StdGen -> Memory -> Memory
withGen g' (Memory gram mods _) = Memory gram mods g'

-- |Return a new 'Memory' with an updated 'Grammar'.
withGrammar :: Grammar -> Memory -> Memory
withGrammar gram' (Memory _ mods g) = Memory gram' mods g