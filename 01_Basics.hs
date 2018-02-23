{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Basics where

import           Debug.Trace -- (traceShow)
import           Data.List
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


prop_const :: Property
prop_const = property $ do
  1 === 1
  failure

prop_sort1 :: Property
prop_sort1 = property $ do
  let xs = [1, 2, 3]
  length (sort xs) === length xs

prop_sort1_2 :: Property
prop_sort1_2 = property $ do
  xs <- forAll $ do
    x <- Gen.int (Range.linear (-50) 50)
    pure [x]
  traceShow xs $
    length (sort xs) === length xs


iAmReady :: IO ()
iAmReady = do
  _ <- checkParallel $$(discover)
  pure ()

-- import Basics
-- check prop_const







-- * oracles

-- if you find an algorithm is too slow, and you think of a faster one, you can
-- keep the old one in your test suite.  sometimes, it's easy to start with a
-- really slow algorithm that is really easy to understand and reason about, and
-- move that to the test suite right away.  this is your self-implemented
-- oracle.

-- | Decide whether two 'Int's with a given sum can be found in an 'Int' list.
-- <https://www.youtube.com/watch?v=XKu_SEDAykw>
--
-- @
-- existsPairSum [1, 2, 3, 9] 8 == False
-- existsPairSum [1, 2, 4, 4] 8 == True
-- existsPairSum [1, 2, 4, 12, 13, 100] 104 == True
-- existsPairSum [0, 0, 2] 2 == True
-- @
existsPairSum :: [Integer] -> Integer -> Bool
existsPairSum xs goal = f xs' (reverse xs')
  where
    xs' = zip [(0 :: Integer)..] $ sort xs

    f [] _ = False
    f _ [] = error "impossible."
    f ((i, _):_) ((i', _):_) | i >= i' = False
    f xs_@((_, x):xs_') ys_@((_, y):ys_') = case x + y of
      shot | shot == goal -> True
           | shot < goal  -> f xs_' ys_
           | otherwise    -> False -- f xs_ ys_'



-- WE WANT Property tests for hte above!!
-- So we make a second Oracle implementation, which is slow, but easier to reason
-- if one dissagrees with the other.. then we can see there's a problem
existsPairSumOracle :: [Integer] -> Integer -> Bool
existsPairSumOracle xs goal = not $ null
  [ (a, b) | a <- xs, b <- xs \\ [a], a + b == goal ]


prop_existsPairSum :: Property
prop_existsPairSum = property $ do
  xs <- forAll $ Gen.list
          (Range.linear 0 100)
          (Gen.integral (Range.linear (-500000) 500000))
  goal <- forAll $ Gen.integral (Range.linear (-500000) 500000)
  -- alternatively... if we aren't finding any goals that work,
  -- we could manually select a goal that will result in true
  --goal <- Gen.choice [ pure . sum $ take 2 xs -- True
  --                   , pure $ 50000 * 2 + 1 -- False
  --                   -- , Gen.integral (Range.linear (-500000, 500000)) -- probably True
  --                   ]

  traceShow (existsPairSum xs goal) $ do
    annotate "web"
    existsPairSum xs goal === existsPairSumOracle xs goal
  
