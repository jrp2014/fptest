{- |
Module      : FPRun
Description : "FPRun" executes IBM floating point test cases either directly,
               or by generating a Haskell script that does it.
Copyright   : (c) John Pavel, 2014
                  IBM Corp 2004
License     : BSD3
Maintainer  : jrp@dial.pipex.com
Stability   : alpha
Portability : portable

See
<https://www.research.ibm.com/haifa/projects/verification/fpgen/papers/ieee-test-suite-v2.pdfi
Floating-Point Test-Suite for IEEE> and
<https://www.research.ibm.com/haifa/projects/verification/fpgen/syntax.txt
Syntax of the Test Cases>

-}


module FPRun where

import FPTypes
import FPParse

import Data.Char (toUpper)
import Data.Either (rights)
import Data.Fixed (divMod')
import Data.List (intercalate)
import Data.Word (Word64, Word32)
import Numeric (floatToDigits, showHex)
import Text.ParserCombinators.Parsec
import Text.Parsec.Number (sign, hexFloat)
import Unsafe.Coerce (unsafeCoerce)

{- |
= Direct test case execution -}


{- | 'Output' represents either an error message (mainly for unimplemented tests)
or a 'RealFloat' value. -}
type ErrMsg = String
type Output a = Either ErrMsg a

{- | 'execute' runs a test case directly, returning either an error message or
the resulting 'RealFloat'('Float' or 'Double') value.
TODO:: sync with 'translate' -}
execute :: (RealFloat a, HasNaN a) => ParsedTestCase -> Output a
execute TestCase { -- format = f,
  operation = op,
  {- roundingMode = rm,
  trappedExceptions = te, -}
  inputs = is
  {- output = o
  outputExceptions = oe -}
  } =
    case op of
      Add -> return $ i1 + i2
      Subtract -> return $ i1 - i2
      Multiply -> return $ i1 * i2
      Divide -> return $ i1 / i2
      FusedMultiplyAdd -> unimplemented
      SquareRoot -> return $ sqrt i1
      Remainder -> return r where (_, r) = i1 `divMod'` i2 -- i1 bound to Integer
      RoundFloatToInteger -> return $ fromIntegral $ round i1 -- defaults to Double
      ConvertFloatToFloat -> return i1 -- ?
      ConvertFloatToInteger -> unimplemented
      ConvertIntegerToFloat -> unimplemented
      ConvertDecimalToString -> unimplemented
      ConvertStringToDecimal -> unimplemented
      QuietComparison -> return $ boolNum (i1 == i2)
      SignallingComparison -> unimplemented
      Copy -> return i1
      Negate -> return $ negate i1
      Abs -> return $ abs i1
      CopySign -> unimplemented {- Copysign(x, y) returns x with the sign of y.
                                Hence, abs(x) = copysign( x, 1.0), even if x is NaN -}
      Scalb -> unimplemented {- Scalb(y, N) returns y × 2N for integral values N
                             without computing 2N -}
      Logb -> return $ log i1
      NextAfter -> unimplemented
      Class -> return $ fpclass i1
      IsSigned -> return $ boolNum $ i1 < 0.0
      IsNormal -> return $ boolNum $ not $ isDenormalized i1
      IsFinite -> return $ boolNum $ not $ isInfinite i1
      IsZero -> return $ boolNum $ i1 == 0.0
      IsSubNormal -> return $ boolNum $ isDenormalized i1
      IsInf -> return $ boolNum $ isInfinite i1
      IsNaN -> return $ boolNum $ isNaN i1
      IsSignalling -> unimplemented
      MinNum -> return $ min i1 i2
      MaxNum -> return $ max i1 i2
      MinNumMag -> return $ min (abs i1) (abs i2)
      MaxNumMag -> return $ max (abs i1) (abs i2)
      SameQuantum -> unimplemented -- only applicable to Decimal operands
      Quantize -> unimplemented -- only applicable to Decimal operands
      NextUp -> unimplemented
      NextDown -> unimplemented
      Equivalent -> unimplemented
    where
      i1 = hexToFloat $ head is
      i2 = hexToFloat $ head $ tail is

      fpclass x
      -- \| x is signalling NaN = 0.0
        | isNaN x = 1.0 -- quiet NaN (x != x)
        | x < 0.0 && isInfinite x = 2.0           -- -INFINITY
        | x < 0.0 && not (isDenormalized x) &&
                     not (isNegativeZero x) = 3.0 -- negative normalized nonzero
        | x < 0.0 && isDenormalized x = 4.0       -- negative denormalized
        | isNegativeZero x = 5.0
        | x == 0.0 = 6.0
        | x > 0.0 && isDenormalized x = 7.0       -- positive denormalized
        | x > 0.0 && not (isDenormalized x) = 8.0 -- positive normalized nonzero
        | x > 0.0 && isInfinite x = 9.0           -- +INFINITY
        | otherwise = 10.0

      unimplemented :: Either ErrMsg a
      unimplemented = Left $ show op ++ " " ++ show is ++ " is unimplemented"

{- |
= Test case translation -}


{- | 'translate' a 'ParsedTestCase' into a Haskell (HUnit) test (or a diagnostic
string, mainly indicating that a test is unimplemented. -}
translate :: ParsedTestCase -> Either String String
translate TestCase { format = f,
operation = op,
  roundingMode = rm,
  trappedExceptions = te,
  inputs = is,
  output = o,
  outputExceptions = oe
  }
  | te /= [] = Left $ "trapped exceptions unimplemented " ++ display te
  | otherwise =
      case f of
        BasicFormat Binary32 -> translate' "F"
        BasicFormat Binary64 -> translate' "D"
        _ -> Left $ show f ++ " is not an implemented format"
      where
        translate' fmt =
            case op of
              Add -> return $ expected ++ i1 ++ " + " ++ i2
              Subtract -> return $ expected ++ i1 ++ " - " ++ i2
              Multiply -> return $ expected ++ i1 ++ " * " ++ i2
              Divide -> return $ expected ++ i1 ++ " / " ++ i2
              FusedMultiplyAdd -> unimplemented
              SquareRoot -> return $ expected ++ "sqrt " ++ i1
              Remainder -> return $ expected ++ "r where (_, r) = " ++ i1 ++ " `divMod'` " ++ i2
              RoundFloatToInteger -> return $ expected ++ "round " ++ i1
              ConvertFloatToFloat -> unimplemented
              ConvertFloatToInteger -> unimplemented
              ConvertIntegerToFloat -> unimplemented
              ConvertDecimalToString -> unimplemented
              ConvertStringToDecimal -> unimplemented
              QuietComparison -> return $ expected ++ "boolNum (" ++ i1 ++ "==" ++ i2 ++ ")"
              SignallingComparison -> unimplemented
              Copy -> return $ expected ++ i1
              Negate -> return $ expected ++ "negate " ++ i1
              Abs -> return $ expected ++ "abs " ++ i1
              CopySign -> unimplemented {- Copysign(x, y) returns x with the sign of y.
                                        Hence, abs(x) = copysign( x, 1.0), even if x is NaN -}
              Scalb -> unimplemented {- Scalb(y, N) returns y × 2N for integral values N
                                     without computing 2N -}
              Logb -> return $ expected ++ "log" ++ i1
              NextAfter -> unimplemented
              Class -> unimplemented
              IsSigned -> return $ expected ++ "boolNum (signum " ++ i1 ++ " == -1.0)"
              -- isNormal(x) is true if and only if x is normal (not zero, subnormal, infinite, or NaN).
              IsNormal -> return $ expected ++ "boolNum (not ( isDenormalized " ++ i1 ++
                 " || isInfinite " ++ i1 ++ "|| isNaN " ++ i1 ++ " || " ++ i1 ++ " == 0 ))"
              -- isFinite(x) is true if and only if x is zero, subnormal or normal (not infinite or NaN).
              IsFinite -> return $ expected ++ "boolNum (not ( isInfinite " ++ i1 ++ " || isNaN " ++ i1 ++ "))"
              IsZero -> return $ expected ++ "boolNum (0 == " ++ i1 ++ ")"
              IsSubNormal -> return $ expected ++ "boolNum ( isDenormalized " ++ i1 ++ ")"
              IsInf -> return $ expected ++ "boolNum (isInfinite " ++ i1 ++ ")"
              IsNaN -> return $ expected ++ "boolNum (isNaN " ++ i1 ++ ")"
              IsSignalling -> unimplemented
              MinNum -> return $ expected ++ "min" ++ i1 ++ i2
              MaxNum -> return $ expected ++ "max" ++ i1 ++ i2
              MinNumMag -> unimplemented
              MaxNumMag -> unimplemented
              SameQuantum -> unimplemented -- only applicable to Decimal operands
              Quantize -> unimplemented -- only applicable to Decimal operands
              NextUp -> unimplemented
              NextDown -> unimplemented
              Equivalent -> unimplemented
            where

              expected = ot ++ " ~==? " -- NaN ~==? NaN is True

              i1 = ft $ showHexToFloat $ head is
              i2 = ft $ showHexToFloat $ head $ tail is

              ot = ft $ showHexToFloat o

              -- show will produce "Infinity", "NaN", etc, so wrap it
              showHexToFloat s
                | s == "+Zero" = "0.0"
                | s == "-Zero" = "-0.0"
                | s == "+Inf" = "1.0/0.0"
                | s == "-Inf" = "(-1.0/0.0)"
                | s == "Q" = "0/0"
                | s == "S" = "0/0" -- for now.  Should really produce Left
                | s == "#" = "-999.0" -- output is ignored, so no matter
                | fmt == "F" = show (hexToFloat s :: Float)
                | otherwise = show (hexToFloat s :: Double)

              ft :: String -> String
              ft s = "(" ++ s ++ "::" ++ fmt ++ ")"

              unimplemented :: Either String String
              unimplemented = Left $ show op ++ " " ++ show is ++ " is unimplemented"

-- | 'checkResult' verifies the result of running a 'ParsedTestCase' directly
checkResult :: ParsedTestCase -> String
checkResult t@TestCase { format = f,
  output = o,
  outputExceptions = oe
  } = display t ++ ": " ++
    case f of
      BasicFormat Binary32 ->
        checkResult' ( execute t :: Output Float )
      BasicFormat Binary64 ->
        checkResult' ( execute t :: Output Double )
      _ -> show f ++ " is an unimplemented floating point format"
    where
      checkResult' e =
        case e of
          Left err -> err -- pass error message on
          Right result -- compare result with expectation
            | o == "#" -> "No output expected: " ++ show result
            | floatToHex result == o -> "Success!"
            | otherwise -> floatToHex result ++ " (" ++ show result ++ ") /= " ++ display o


-- 'translateResult' turns a 'ParsedTestCase' into an HUnit test or a diagnostic String
translateResult :: ParsedTestCase -> Either String String
translateResult t@TestCase { format = f,
  operation = op,
  {- roundingMode = rm,
  trappedExceptions = te, -}
  inputs = is,
  output = o
  -- outputExceptions = oe
  } =
    case translate t of
    Left diagnostic -> Left diagnostic
    Right testtext -> return $ "\" " ++ display t ++ "\" ~: (" ++ testtext ++ ")"


-- 'strcmp' provides insensitive string comparison (unused)
strcmp :: String -> String -> Bool
strcmp [] [] = True
strcmp s1 s2 = case (s1, s2) of
  (s11 : ss1, s21 : ss2)
    | toUpper s11 == toUpper s21 -> strcmp ss1 ss2
    | otherwise -> False
  _ -> False


{- |
== Helper functions for hex floating point format literals.
TODO:: This does not yet work with IBM's hex floating point literal format -}

-- | 'hexToFloat' parses a string into a 'RealFloat'.
hexToFloat :: (RealFloat a, HasNaN a) => String -> a
hexToFloat s
  | s == "+Zero" = 0.0
  | s == "-Zero" = -0.0
  | s == "+Inf" = 1 / 0
  | s == "-Inf" = negate 1.0 / 0.0
  | s == "Q" = quietNaN -- 0 / 0
  | s == "S" = signallingNaN
  | s == "true" = 1.0
  | s == "false" = 0.0
  | otherwise =
      case parse hexToFloat' "hexToFloat" s of
        Left err -> error ("Invalid hex format floating point number '" ++ s ++ "' :" ++ show err)
        Right f -> f
      where
        hexToFloat' :: RealFloat a => GenParser Char st a
        hexToFloat' = do
          sn <- sign
          h <- hexFloat False
          eof
          return $ sn $ case h of
              Left i -> fromInteger i
              Right f -> f


{- | 'floatToHex' takes a 'RealFloat' and produces a hex literal string
 - This uses <http://en.wikipedia.org/wiki/Single-precision_floating-point_formati
 - Converting from decimal representation to binary32 format>
 - TODO:: handle denormalized case
TODO:: refactor this in terms of encodeFloat, signCharificand, etc ? -}
floatToHex :: (RealFloat a, Show a, HasNaN a) => a -> String
floatToHex x
  | isNaN x = "Q" -- Haskell seems to use only quiet NaNs
  | isInfinite x && x > 0 = "+Inf"
  | isInfinite x = "-Inf"
  | isNegativeZero x = "-Zero"
  | x == 0.0 = "+Zero"
-- | isDenormalized x = "Denormalized"
  | not (isIEEE x) = "Not an IEEE floating point number: " ++ show x
  | x < 0 = '-' : binaryDigitsToString (floatToDigits 2 (-x))
  | otherwise = binaryDigitsToString (floatToDigits 2 x)
    where
      binaryDigitsToString :: ([Int], Int) -> String
      binaryDigitsToString (1 : normalizedBinaryFraction, n ) = "1." ++
        normalizedBinaryFractionToString (normalizedBinaryFraction, n - 1)
      binaryDigitsToString ([0], _ ) = "Zero"
      binaryDigitsToString ( 0 : denormalizedBinaryFraction, _) = "Denormalized"
      binaryDigitsToString _ = error "binaryDigitsToString"

      normalizedBinaryFractionToString :: ([Int], Int) -> String
      normalizedBinaryFractionToString (nbf, n) =
        binaryToHex nbf ++ "P" ++ exponentSign n ++ show n
          where
            exponentSign s -- add a + sign for +ve exponents
              | s < 0 = ""
              | otherwise = "+"

      binaryToHex :: [Int] -> String
      binaryToHex (a : b : c : d : es) =
        showHex (8 * a + 4 * b + 2 * c + d) (binaryToHex es)
      binaryToHex [a, b, c] = showHex (8 * a + 4 * b + 2 * c) ""
      binaryToHex [a, b] = showHex (8 * a + 4 * b) ""
      binaryToHex [a] = showHex (8 * a) ""
      binaryToHex [] = ""

-- | == Checks

-- | QuickCheck test
prop_FloatHex :: (RealFloat a, HasNaN a, Show a) => a -> Bool
prop_FloatHex f = hexToFloat (floatToHex f) == f

-- | 'check2' parse and check a couple of test cases
check2 :: String
check2 =
  case parse testCaseSpec "" "b32?i =0 +1.000000P0 -> 0x0\nb32+ =0 i +0.000001P-126 +1.000000P-126 -> +1.000001P-126" of
  Left err -> show err
  Right t -> checkResult t

{- |
= Helper functions -}

 {- Some example code that allows NaNs to signal, from
 <http://stackoverflow.com/questions/21344139/ieee-floating-point-signalling-nan-snan-in-haskell>
 Doesn't do us much good here, as Haskell does not produce signalling NaNs at all -}


{- | 'HasNaN' gives two values (signalling and quiet NaN.
At present, although the values can be set, Haskell doesn't generate signalling NaNs -}
class (RealFloat a, Show a) => HasNaN a where
    signallingNaN :: a
    quietNaN :: a

instance HasNaN Double where
    signallingNaN = unsafeCoerce (0x7ff4000000000000 :: Word64)
    quietNaN = unsafeCoerce (0x7ff8000000000000 :: Word64)

instance HasNaN Float where
    signallingNaN = unsafeCoerce (0x7fa00000 :: Word32)
    quietNaN = unsafeCoerce (0x7fc00000 :: Word32)


-- | 'boolNum' is a helper function that simply translates True / False to 1.0/0.0
boolNum :: RealFloat f => Bool -> f
boolNum True = 1.0
boolNum False = 0.0


{- |
= Running the test cases in a list of IBM floating point test specification files -}


{- | 'evalTests' takes a list of IBM test case files (.fptest) and executes
them directly (interprets them), printing the results -}
evalTests :: [FilePath] -> IO ()
evalTests = mapM_ evalTestCases
  where
    evalTestCases :: String -> IO ()
    evalTestCases fileName = do
      result <- parseFromFile testCaseFile fileName
      putStrLn fileName
      case result of
        Left err -> do
                      print err
                      error "Test case file failed to parse"
        -- Right xs -> mapM_ print (map checkResult xs)
        Right xs -> putStrLn $ unlines (map checkResult xs)

{- | 'translateTests' takes a list of IBM test cases and translates their contents
into a set of Haskell (HUnit) tests that can be run separately. -}
translateTests :: [FilePath] -> IO ()
translateTests = mapM_ translateTestCases
  where
    translateTestCases :: FilePath -> IO ()
    translateTestCases fileName = do
      result <- parseFromFile testCaseFile fileName
      case result of
        Left err -> do  -- unsuccessful parse
                      print err
                      error "Test case file failed to parse"
        Right xs -> do  -- successful parse
          preface
          putStr $ "tests = " ++ show fileName ++ " ~: [\n  "
          {- Take only the rights, ignoring the lefts, which contain error messages,
          indicating that a test is unimplemented, eg. -}
          putStr $ intercalate ",\n  " (rights (map translateResult xs))
          putStrLn "]"


{- | 'preface' outputs some boilerplate code for placing at the head of
of a file of test case translations in Haskell (HUnit) format -}
preface :: IO ()
preface = mapM_ putStrLn [
  "module FPTestTest where\n\n",
  "import Test.HUnit\n",
  "type D = Double",
  "type F = Float\n",
  "boolNum :: RealFloat a => Bool -> a",
  "boolNum x",
  "  | x = 1",
  "  | otherwise = 0\n",

  "-- Version that treats NaN == NaN",
  "assertEqual' :: (Eq a, Show a, RealFloat a) => String -> a -> a -> Assertion",
  "assertEqual' preface expected actual =",
  " if (isNaN actual && isNaN expected) || (actual == expected)",
  " then return () else assertFailure msg",
  "    where msg = (if null preface then \"\" else preface ++ \"\\n\") ++",
  "                \"expected: \" ++ show expected ++ \"\\n but got: \" ++ show actual",
  "",
  "infix 1 @==?, @?==, ~==?, ~?==",
  "",
  "(@==?) :: (Eq a, Show a, RealFloat a) => a -> a -> Assertion",
  "expected @==? actual = assertEqual' \"\" expected actual",
  "",
  "(@?==) :: (Eq a, Show a, RealFloat a) => a -> a -> Assertion",
  "actual @?== expected = assertEqual' \"\" expected actual",
  "",
  "(~==?) :: (Eq a, Show a, RealFloat a) => a -> a -> Test",
  "expected ~==? actual = TestCase (expected @==? actual)",
  "",
  "(~?==) :: (Eq a, Show a, RealFloat a) => a -> a -> Test",
  "actual ~?== expected = TestCase (actual @?== expected)",
  "",

  "main :: IO Counts",
  "main = runTestTT tests"
    ]
