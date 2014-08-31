import Data.Either
import Test.QuickCheck
import Foreign.C.Types
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2

{- From IEEE Standard 754 for Binary Floating-Point Arithmetic
by
Prof. W. Kahan
Elect. Eng. & Computer Science
University of California
Berkeley CA 94720-1776

http://www.eecs.berkeley.edu/~wkahan/ieee754status/IEEE754.PDF -}

{- This function computes the roots x1 and x2 of
Quadradic of the form p*x^2 - 2 q*x + r == 0
as accuractely as they are determined by p, q, r -}
qdrtc :: RealFloat a => a -> a -> a -> (a, a)
qdrtc p q r
  | ss == 0 = (r / p, r / p)
  | otherwise = (r / ss, ss / p)
  where
    s = sqrt (q * q - p * r) -- NaN if sqrt (< 0)
    ss = q + copysign s q

floatQdrtc :: Float -> Float -> Float -> (Float, Float)
floatQdrtc = qdrtc
doubleQdrtc :: Double -> Double -> Double -> (Double, Double)
doubleQdrtc = qdrtc
cfloatQdrtc :: CFloat -> CFloat -> CFloat -> (CFloat, CFloat)
cfloatQdrtc = qdrtc
cdoubleQdrtc :: CDouble -> CDouble -> CDouble -> (CDouble, CDouble)
cdoubleQdrtc = qdrtc


{- copysign(x, y) returns x with the sign of y.
Hence, abs(x) = copysign( x, 1.0), even if x is NaN
NB: Haskell's signum returns signum NaN -> -1.0 -}
copysign :: RealFloat a => a -> a -> a
copysign x y
  | isNaN y = 0 / 0
  | signum x == signum y = x
  | otherwise =  -x


log2 :: RealFloat a => a -> a
log2 x = logBase 2 (abs x)

-- Haskell's min is not symmetric in the presence of a NaN
min' x y
  | isNaN x || isNaN y = 0 / 0
  | otherwise = min x y

qtrial :: (RealFloat a, Show a) => a -> Either String (a, String)
qtrial r
  | p <= 0 = Left "qtrail expects r > 2"
  | not ((r - q) == 1.0 && (q - p) == 1.0) = Left $ "r =" ++
      show r ++ " is too big for qtrail"
  | x1 < 1.0 = Left $ "r = " ++ show r ++ " fails: root " ++ show x1 ++
      " isn't at least 1"
  | otherwise = return (min' e1 e2, -- min' NaN NaN = NaN
                        "r = " ++ show r ++ " produces " ++ show e1 ++
                          " and " ++ show e2 ++ " sig. bits" )
  where
    p = r - 2.0
    q = r - 1.0

    (x1, x2) = qdrtc p q r

    e1 = - log2 (x1 - 1.0) -- Could be NaN
    e2 = - log2 ((x2 - 1.0) - 2.0 / p)

-- Runs the trial against a series of different values of r
qtestraw :: (RealFloat a, Show a) => [Either String (a, String)]
qtestraw = map qtrial r
  where
  r =
    [2 ^ 12 + 2.0, -- for 24 sig. bits,
     2 ^ 12 + 2.25, -- and 6 hex.
     16 ^ 3 + 1.0 + 1.0 / 16 ^ 2, -- 6 hex. IBM.
     2 ^ 24 + 2.0, -- 48 bits CRAY -
     2 ^ 24 + 2.25, -- rounded;
     2 ^ 24 + 3.0, -- 48 bits chopped.
     94906267.0, -- 53 sig. bits.
     94906267 + 0.25, -- 53 sig. bits.
     2 ^ 28 - 5.5, -- PowerPC, i860.
     2 ^ 28 - 4.5, -- PowerPC, i860.
     2 ^ 28 + 2.0, -- 56 sig. bits,
     2 ^ 28 + 2.25, -- and 14 hex.
     16 ^ 7 + 1.0 + 1.0 / 16 ^ 6, -- 14 hex. IBM.
     2 ^ 32 + 2.0, -- 64 sig. bits.
     2 ^ 32 + 2.25] -- 64 sig. bits.

-- Generates a more readable interpretation of the raw results of a trail
qtest :: (RealFloat a, Show a) => [Either String (a, String)] -> String
qtest raw = unlines (map (either id snd) raw) ++
    "Worst accuracy is " ++
    show (minimum (map fst (rights raw))) ++ " sig. bits"


{- QuickCheck testing
Illustrates numerical instability -}
floatQTest :: [Either String (Float, String)]
floatQTest = qtestraw
doubleQTest :: [Either String (Double, String)]
doubleQTest = qtestraw
cfloatQTest :: [Either String (CFloat, String)]
cfloatQTest = qtestraw
cdoubleQTest :: [Either String (CDouble, String)]
cdoubleQTest = qtestraw

-- Approximately equal, ignoring isNaN
-- TODO:: overload for tuples, reduce tolerace to 1e-15 for doubles
infix 4 ~==
(~==) :: RealFloat a => a -> a -> Bool
x ~== y 
  | isNaN x || isNaN y = True
  | otherwise = abs (x - y) < 1.0e-6 * abs x

test1 f p = p /= 0 ==> f p 0 0 == (0, 0)
prop_test1f = test1 floatQdrtc
prop_test1d = test1 doubleQdrtc
prop_test1cf = test1 cfloatQdrtc
prop_test1cd = test1 cdoubleQdrtc

test2 f = f 2.0 5.0 12.0 == (2, 3)
prop_test2f = test2 floatQdrtc
prop_test2d = test2 doubleQdrtc
prop_test2cf = test2 cfloatQdrtc
prop_test2cd = test2 cdoubleQdrtc

test3 f = f 2.0e-37 1.0 2.0 == (1.0, 1.0e37)
prop_test3f = test3 floatQdrtc
prop_test3d = test3 doubleQdrtc
prop_test3cf = test3 cfloatQdrtc
prop_test3cd = test3 cdoubleQdrtc

test4 f p q r =
  p /= 0 ==> x1 ~== 1.0 / x2' && x2 ~== 1.0 / x1'
  where
    (x1, x2) = f p q r
    (x1', x2') = f r q p
prop_test4f = test4 floatQdrtc
prop_test4d = test4 doubleQdrtc
prop_test4cf = test4 cfloatQdrtc
prop_test4cd = test4 cdoubleQdrtc

test5 f mu p q r = (mu * p) /= 0 ==> x1 ~== x1' && x2 ~== x2'
  where
    (x1, x2) = f p q r
    (x1', x2') = f (mu * p) (mu * q) (mu * r)
prop_test5f = test5 floatQdrtc
prop_test5d = test5 doubleQdrtc
prop_test5cf = test5 cfloatQdrtc
prop_test5cd = test5 cdoubleQdrtc

-- (Orphan) instances, as they are defined neither in Foreign.C.Types,
-- nor in Test.QuickCheck

instance Arbitrary CFloat where
  arbitrary = arbitrarySizedFractional
  shrink    = shrinkRealFrac

instance Arbitrary CDouble where
  arbitrary = arbitrarySizedFractional
  shrink    = shrinkRealFrac


-- Gather the tests

tests = [
    testGroup "test1" [
      testProperty "Float" prop_test1f,
      testProperty "Double" prop_test1d,
      testProperty "CFloat" prop_test1cf,
      testProperty "CDouble" prop_test1cd
    ],
    testGroup "test2" [
      testProperty "Float" prop_test2f,
      testProperty "Double" prop_test2d,
      testProperty "CFloat" prop_test2cf,
      testProperty "CDouble" prop_test2cd
    ],
    testGroup "test3" [
      testProperty "Float" prop_test3f,
      testProperty "Double" prop_test3d,
      testProperty "CFloat" prop_test3cf,
      testProperty "CDouble" prop_test3cd
    ],
    testGroup "test4" [
      testProperty "Float" prop_test4f,
      testProperty "Double" prop_test4d,
      testProperty "CFloat" prop_test4cf,
      testProperty "CDouble" prop_test4cd
    ],
    testGroup "test5" [
      testProperty "Float" prop_test5f,
      testProperty "Double" prop_test5d,
      testProperty "CFloat" prop_test5cf,
      testProperty "CDouble" prop_test5cd
    ]
  ]


-- Run all the tests

--main = defaultMain tests


main :: IO ()
main = do

  putStrLn "\nResults for Float:"
  putStrLn $ qtest floatQTest

  putStrLn "\nResults for CFloat:"
  putStrLn $ qtest cfloatQTest

  putStrLn "\nResults for Double:"
  putStrLn $ qtest doubleQTest

  putStrLn "\nResults for CDouble:"
  putStrLn $ qtest cdoubleQTest

  putStrLn "\n\nUnit Tests:"
  defaultMain tests
