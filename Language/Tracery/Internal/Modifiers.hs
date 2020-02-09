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

toModifiers :: [(String, Modifier)] -> Modifiers
toModifiers = HM.fromList . Prelude.map (\(name, modifier) -> (Mod name, modifier))

serializeMods :: [ModifierName] -> [Text]
serializeMods (Mod m : rest) = pack ('.' : m) : serializeMods rest
serializeMods [] = []

addModifiers :: Modifiers -> Modifiers -> Modifiers
addModifiers = HM.union

defaultModifiers :: HM.HashMap ModifierName Modifier
defaultModifiers = toModifiers [
        ("c", capitalize)
    ]

applyModifier :: Modifiers -> ModifierName -> Text -> Text
applyModifier mods m = fromMaybe id (HM.lookup m mods)

capitalize :: Text -> Text
capitalize text = pack $
    case unpack text of
        c : rest -> Char.toUpper c : rest
        [] -> []

instance Hashable ModifierName where
    hashWithSalt salt (Mod s) = hashWithSalt salt s

instance Show ModifierName where
    show (Mod m) = show ("." ++ m)

