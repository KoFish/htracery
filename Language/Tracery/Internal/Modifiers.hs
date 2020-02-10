module Language.Tracery.Internal.Modifiers where

import qualified Data.Char as Char
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Text

newtype ModifierName = Mod String
  deriving( Eq )

type Modifier = Text -> Text
type Modifiers = HM.HashMap ModifierName Modifier

-- |Convert a set of named 'Modifier's to a format that can be used by a 'Memory'.
toModifiers :: [(String, Modifier)] -> Modifiers
toModifiers = HM.fromList . Prelude.map (\(name, modifier) -> (Mod name, modifier))

serializeMods :: [ModifierName] -> [Text]
serializeMods (Mod m : rest) = pack ('.' : m) : serializeMods rest
serializeMods [] = []

-- |Combine two sets of 'Modifiers'.
addModifiers :: Modifiers -> Modifiers -> Modifiers
addModifiers = HM.union

-- |Default english 'Modifiers' for tracery.
englishModifiers :: HM.HashMap ModifierName Modifier
englishModifiers = toModifiers
    [ ("c", capitalize)  -- Make the first character uppercase
    , ("t", titleCase)   -- Make the first character of every word uppercase
    , ("a", an)          -- Prefix word with a/an
    , ("s", pluralize)   -- Suffix word with correct pluralization
    , ("ed", past)       -- Change the word to past tense
    , ("lower", toLower) -- Make all characters lower case
    , ("upper", toUpper) -- Make all characters upper case
    ]

applyModifier :: Modifiers -> ModifierName -> Text -> Text
applyModifier mods m = fromMaybe id (HM.lookup m mods)

-- |'c' - Capitalize first letter
capitalize :: Text -> Text
capitalize text = pack $ capitalize' (unpack text)

capitalize' :: String -> String
capitalize' (c:rest) = Char.toUpper c : rest
capitalize' [] = []

-- |'t' - Capitalize first letter of every word
titleCase :: Text -> Text
titleCase text = pack $ Prelude.unwords [capitalize' t | t <- Prelude.words (unpack text)]

-- |'a' - Change word to object form
an :: Text -> Text
an text = pack $
    case unpack (toLower text) of
        'u':_:'i':_                    -> "a " ++ unpack text
        a:_         | a `elem` "aeiou" -> "an " ++ unpack text
        _                              -> "a " ++ unpack text

withReversed :: Text -> (String -> String) -> String
withReversed text f =
    let reversed = Data.Text.reverse (toLower text)
    in f (unpack reversed)

-- |'s' - Change word to plural form
pluralize :: Text -> Text
pluralize text = pack $
    let t = unpack text
    in withReversed text (\case
        x:_     | x `elem` "shx"      -> t ++ "es"
        'y':x:_ | x `notElem` "aeiou" -> Prelude.init t ++ "ies"
        _                             -> t ++ "s"
    )

-- |'ed' - Change the word to past tense
past :: Text -> Text
past text = pack $
    let t = unpack text
    in withReversed text (\case
        'e':_                         -> t ++ "d"
        'y':x:_ | x `notElem` "aeiou" -> Prelude.init t ++ "ied"
        _                             -> t ++ "ed"
    )

instance Hashable ModifierName where
    hashWithSalt salt (Mod s) = hashWithSalt salt s

instance Show ModifierName where
    show (Mod m) = show ("." ++ m)

