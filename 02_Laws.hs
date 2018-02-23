{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | polymorphic types and type classes often have laws that are expected to
-- hold for all their instances.  these laws are nothing *but* properties, so
-- let's test some of them!
module Laws where

import           Control.Lens
import           Data.Monoid
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


go :: IO ()
go = do
  _ <- checkSequential $$(discover)
  pure ()


-- * lens laws

-- | special case: @_1 = Lens' (Bool, Int) Bool@
--
-- intuitively (and not very accurately), a lens is a pair of getter, setter for
-- some type.  for what we need in the following, you can read the following as
-- language in itself, without understanding how exactly it works in the Haskell
-- semanics:
--
-- >>> _1 :: Lens (a, b) a
-- >>> *Laws> (1, False) ^. _1
-- >>> 1
-- >>> *Laws> (False, 1) ^. _1
-- >>> False
-- >>> *Laws> (False, 1) & _1 .~ True
-- >>> (True,1)
--
-- all lenses have to satisfy a number of properties (laws)
-- <https://hackage.haskell.org/package/lens-4.16/docs/Control-Lens-Type.html#t:Lens>
--
-- - You get back what you put in
-- - Putting back what you got doesn't change anything
-- - Setting twice is the same as setting once
--
--
prop_Lens1 :: Property
prop_Lens1 = property $ do
  s :: (Bool, Int) <- forAll $ do -- ((,) <%> Gen.bool :: Int -> (Bool, Int)) <*> Gen.int (Range.linearBounded)

  -- remember these guys vv
  -- (<$> :: (a -> b) -> f a -> f b
  -- (<*> :: f (a -> b) -> f a -> f b
    b <- Gen.bool
    i <- Gen.int (Range.linearBounded)
    pure (b, i)
  v :: Bool <- forAll Gen.bool
  v' :: Bool <- forAll Gen.bool

  let lns :: Lens' (Bool, Int) Bool
      lns = _1

  view lns (set lns v s) === v
  set lns (view lns s) s === s
  set lns v' (set lns v s) === set lns v' s

lensLaws :: (Eq s, Eq v, Show s, Show v) =>Lens' s v -> Gen s -> Gen v -> Property
lensLaws lns sgen vgen = property $ do
  s  <- forAll sgen
  v  <- forAll vgen
  v' <- forAll vgen

  view lns (set lns v s) === v
  set lns (view lns s) s === s
  set lns v' (set lns v s) === set lns v' s

prop_Lens2 :: Property
prop_Lens2 = lensLaws _1 ((,) <$> Gen.bool <*> Gen.bool) Gen.bool

prop_Lens3 :: Property
prop_Lens3 = lensLaws _2 ((,) <$> Gen.bool <*> Gen.bool) Gen.bool

-- * monoid laws

-- | this is almost unchanged cut & paste from
-- <https://hackage.haskell.org/package/base/docs/Data-Monoid.html>!
--
monoidLaws :: (Monoid m) => Gen m -> Property
monoidLaws = undefined
-- monoidLaws mgen = property $ do
-- m <- mgen
-- n <- mgen
-- m <> mempty === mempty <> m



prop_MonoidIntList :: Property
prop_MonoidIntList = undefined


-- | another example
--
-- >>> *Laws>  Last Nothing <> Last Nothing
-- >>> Last (Nothing)
-- >>> *Laws> Last Nothing <> Last (Just True)
-- >>> Last (Just True)
-- >>> *Laws>  Last (Just True) <> Last Nothing
-- >>> Last (Just True)
-- >>> *Laws>  Last (Just True) <> Last Nothing <> Last (Just False) <> Last Nothing
-- >>> Last (Just False)
--
prop_MonoidLast :: Property
prop_MonoidLast = undefined
