{-|
Module      : Language.Tracery
Description : Haskell implementation of Tracery.io
Copyright   : (c) Krister Svanlund, 2020
License     : MIT
Maintainer  : kofish@mailbox.org
Stability   : experimental

The basic idea of tracery is to define a 'Grammar' in json that consist keys mapped to
lists of sentences. Each time a key is evaluated a random sentence is picked and that
sentence is, in turn, evaluated.

Sentences can contain actions surrounded by '#', actions can either refer to a different
key that should be evaluted and replace the action in the evaluated sentence, or it can
store the expansion of a sentence in a key.

Examples:

- The sentence 'This is a \#object\#' will replace '#object#' with whatever the key 'object'
  is evaluated to.

- The sentence '\#[foo:\#object\#]\#' will store the evaluation of '#object#' in the key 'foo'
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Tracery
    ( loadGrammar
    , parseGrammar
    , initMemory
    , addModifiers
    , toModifiers
    , englishModifiers
    , lookup
    , tracery
    , runTracery
    ) where

import Language.Tracery.Internal.Memory(initMemory)
import Language.Tracery.Internal.Modifiers(addModifiers, toModifiers, englishModifiers)
import Language.Tracery.Internal.Loader(loadGrammar, parseGrammar)
import Language.Tracery.Internal.Eval(tracery, runTracery)
import Language.Tracery.Internal.Grammar(lookup)
