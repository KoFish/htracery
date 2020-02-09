{-# OPTIONS_GHC -Wno-orphans #-}
module Arbitraries where

import qualified Data.CharSet as CS
import qualified Data.CharSet.Common as CharSets
import           Language.Tracery.Internal.Grammar
import           Language.Tracery.Internal.Modifiers
import           Test.QuickCheck

shorteningFactor :: Int
shorteningFactor = 5

genLiteral :: Gen LiteralComp
genLiteral = do
    notEmpty <- (getPrintableString <$> arbitrary) `suchThat` (not . null)
    return $ fromString notEmpty

genSymbolLike :: Gen String
genSymbolLike = listOf anAlphaNum `suchThat` (not . null)
  where anAlphaNum = elements (CS.toList CharSets.alphaNum)

genStore :: Int -> Gen ActionComp
genStore n = do
    sym <- arbitrary
    Store sym <$> genShortSentence (n + 1)

genExpand :: Gen ActionComp
genExpand = Expand <$> arbitrary

genShortSentence :: Int -> Gen Sentence
genShortSentence n | n < shorteningFactor = do
    comp <- frequency [(n, Literal <$> genLiteral)
                      ,(1, Action <$> genWeightAction (n + 1))
                      ]
    (Sentence rest) <- genShortSentence (n + 1)
    return $ Sentence (comp : rest)
genShortSentence _ = do
    lits <- listOf (Literal <$> genLiteral)
    return $ Sentence lits

genWeightAction :: Int -> Gen ActionComp
genWeightAction n =
    frequency $ (n, genExpand) :
        [(1, genStore (n + shorteningFactor)) | n < shorteningFactor]

stripActions :: [Component] -> [Component]
stripActions (Action _ : rest) = stripActions rest
stripActions (l@(Literal _) : rest) = l : stripActions rest
stripActions [] = []

instance Arbitrary Sentence where
    arbitrary = Sentence <$> arbitrary
    shrink (Sentence sen) =
        emptySentence :
        (Sentence $ stripActions sen) :
        [Sentence sen' | sen' <- shrink sen]

instance Arbitrary Component where
    arbitrary = oneof [Action <$> genWeightAction 1
                      ,Literal <$> genLiteral]

instance Arbitrary ActionComp where
    arbitrary = genWeightAction 1
    shrink (Store sym sen) =
        Store sym emptySentence :
        [Store sym sen' | sen' <- shrink sen] ++
        [Store sym' sen | sym' <- shrink sym]
    shrink (Expand sym) =
        [Expand sym]

instance Arbitrary SymbolRef where
    arbitrary = arbitrary >>= \sym -> Sym sym <$> arbitrary
    shrink (Sym sym mods) =
        Sym sym [] :
        [Sym sym' mods | sym' <- shrink sym] ++
        [Sym sym mods' | mods' <- shrink mods]

instance Arbitrary Symbol where
    arbitrary = Symbol <$> genSymbolLike

instance Arbitrary ModifierName where
    arbitrary = Mod <$> genSymbolLike
