module Arbitraries where

import Test.QuickCheck
import Language.Tracery.Internal.Grammar
import qualified Data.CharSet as CS
import qualified Data.CharSet.Common as CharSets
import qualified Data.Set as S

shorteningFactor :: Int
shorteningFactor = 5

genLiteral :: Gen Literal
genLiteral = do
    notEmpty <- (getPrintableString <$> arbitrary) `suchThat` (not . null)
    return $ fromString notEmpty

genStore :: Int -> Gen Action
genStore n = do
    sym <- arbitrary
    Store sym <$> genShortSentence (n + 1)

genExpand :: Gen Action
genExpand = Expand <$> arbitrary

genShortSentence :: Int -> Gen Sentence
genShortSentence n | n < shorteningFactor = do
    comp <- frequency [(n, Lit <$> genLiteral)
                      ,(1, Act <$> genWeightAction (n + 1))
                      ]
    (Sentence rest) <- genShortSentence (n + 1)
    return $ Sentence (comp : rest)
genShortSentence _ = do
    lits <- listOf (Lit <$> genLiteral)
    return $ Sentence lits

genWeightAction :: Int -> Gen Action
genWeightAction n =
    frequency $ (n, genExpand) :
        [(1, genStore (n + shorteningFactor)) | n < shorteningFactor]

stripActions :: [Component] -> [Component]
stripActions (Act _ : rest) = stripActions rest
stripActions (l@(Lit _) : rest) = l : stripActions rest
stripActions [] = []

instance Arbitrary Sentence where
    arbitrary = Sentence <$> arbitrary
    shrink (Sentence sen) =
        emptySentence :
        (Sentence $ stripActions sen) :
        [Sentence sen' | sen' <- shrink sen]

instance Arbitrary Modifier where
    arbitrary = elements [Capitalize, Pluralize, AAn]

instance Arbitrary Component where
    arbitrary = oneof [Act <$> genWeightAction 1
                      ,Lit <$> genLiteral]

instance Arbitrary Action where
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
        Sym sym (S.fromList []) :
        [Sym sym' mods | sym' <- shrink sym] ++
        [Sym sym mods' | mods' <- shrink mods]

instance Arbitrary Symbol where
    arbitrary = do
        notEmpty <- listOf anAlphaNum `suchThat` (not . null)
        return $ fromString notEmpty
      where anAlphaNum = elements (CS.toList CharSets.alphaNum)