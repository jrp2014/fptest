{- |
Module      : FPTypes
Description : "FPTypes" defines the types needed to represent floating point test cases.
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


module FPTypes where

import Data.List (intercalate)

-- |
-- = Test case specification rendering

-- | 'Display' is used to 'show' values in the original test suite notation
class Show a => Display a where
  {- | 'display' shows a test specification element in test suite notation
  (defaulting to Haskell show notation if a specialisation
  is not provided explicitly) -}
  display :: a -> String
  display = show

  -- | 'display' a list of test specification elements
  displayList :: [a] -> String
  displayList list = unwords (map display list)

  -- | 'printd' wraps display in 'putStrLn'
  printd :: a -> IO ()
  printd = putStrLn . display

instance Display a => Display [a] where
  display = displayList

instance Display Char where
  displayList = id


-- |
-- = Test case specification

-- | 'TestCase' provides a test case specification
data TestCase operandType = TestCase {
  format :: Format,
  operation :: Operation,
  roundingMode :: RoundingMode,
  trappedExceptions :: [TrappedException],
  inputs :: [operandType],
  output :: operandType,
  outputExceptions :: [Exception]
  } deriving (Show)

{- | 'ParsedTestCase' retains the operands and expected output as
the original strings -}
type ParsedTestCase = TestCase String


instance Display a => Display (TestCase a) where
  display TestCase {
  format = f,
  operation = op,
  roundingMode = rm,
  trappedExceptions = te,
  inputs = ins,
  output = out,
  outputExceptions = oe} =
    display f ++ "{" ++ display op ++ "}" ++ display rm ++ " " ++ display te ++ " " ++ display ins ++ " -> "
      ++ display out ++ " " ++ display oe

  displayList list = intercalate "\n" (map display list)



{- | Tests can use either on of the basic floating point formats from
IEEE754R or they can be based on a pair of such formats (for comparison
tests). -}
data Format = BasicFormat BasicFormat |
  ComparisonFormat BasicFormat BasicFormat deriving (Show, Eq)

instance Display (Format) where
  display (BasicFormat bf) = display bf
  display (ComparisonFormat bf1 bf2) = display bf1 ++ display bf2


{- | 'BasicFormat' represents IEE754R basic formats.
Only 'Binary32' (@b32@ in test specification notation, @Float@)
and 'Binary64' (@b64@,  @Double@) tests are implemented.
The binary IBM sample scripts are 'Binary32'only.
'Decimal32 is not required by IEEE754R -}
data BasicFormat = Binary32 | Binary64 | Binary128 |
  Decimal32 | Decimal64 | Decimal128 deriving (Show, Eq)

instance Display (BasicFormat) where
  display Binary32 = "b32"
  display Binary64 = "b64"
  display Binary128 = "b128"
  display Decimal32 = "d32"
  display Decimal64 = "d64"
  display Decimal128 = "d128"

{- | The 'Operation' under test (add, subtract, multiply, divide,
fused multiply-add, square root, remainder, compare, min, max,
round to integer, convert float to float, convert float to integer,
convert integer to float, and convert and round float to integral value,
etc).  For arithmetic operations the operands are assumed to have the
same format. -}
data Operation = Add | Subtract | Multiply | Divide | FusedMultiplyAdd |
  SquareRoot | Remainder | RoundFloatToInteger |
  ConvertFloatToFloat | ConvertFloatToInteger | ConvertIntegerToFloat |
  ConvertDecimalToString | ConvertStringToDecimal |
  QuietComparison | SignallingComparison |
  Copy | Negate | Abs | CopySign | Scalb | Logb | NextAfter | Class |
  IsSigned | IsNormal | IsFinite | IsZero | IsSubNormal |
  IsInf | IsNaN | IsSignalling |
  MinNum | MaxNum | MinNumMag | MaxNumMag | SameQuantum | Quantize |
  NextUp | NextDown | Equivalent deriving (Show)

{- | For the moment, 'display' the 'Operation' in Haskell format as it is less
cryptic than the native test specification format -}
instance Display (Operation) where
  display = show
{- display Add = "+"
display Subtract = "-"
display Multiply = "*"
display Divide = "/"
display FusedMultiplyAdd = "*+"
display SquareRoot = "V"
display Remainder = "%"
display RoundFloatToInteger = "rfi"
display ConvertFloatToFloat = "cff"
display ConvertFloatToInteger = "cfi"
display ConvertIntegerToFloat = "cif"
display ConvertDecimalToString = "cfd"
display ConvertStringToDecimal = "cdf"
display QuietComparison = "qC"
display SignallingComparison = "sC"
display Copy = "cp"
display Negate = "~"
display Abs = "A"
display CopySign = "@"
display Scalb = "S"
display Logb = "L"
display NextAfter = "Na"
display Class = "?"
display IsSigned = "?-"
display IsNormal = "?n"
display IsFinite = "?f"
display IsZero = "?0"
display IsSubNormal = "?s"
display IsInf = "?i"
display IsNaN = "?N"
display IsSignalling = "?sN"
display MinNum = "<C"
display MaxNum = ">C"
display MinNumMag = "<A"
display MaxNumMag = ">A"
display SameQuantum = "=quant"
display Quantize = "quant"
display NextUp = "Nu"
display NextDown = "Nd"
display Equivalent = "eq" -}

{- | 'RoundingMode' specifies the rounding mode to be used when executing the test.
It is currently unimplemented as Haskell provides no way of changing rounding
mode. -}
data RoundingMode = PositiveInfinity | NegativeInfinity | Zero | NearestEven |
  NearestAwayFromZero deriving (Show, Eq)

instance Display (RoundingMode) where
  display PositiveInfinity = ">"
  display NegativeInfinity = "<"
  display Zero = "0"
  display NearestEven = "=0"
  display NearestAwayFromZero = "^0"

{- | 'TrappedException' specifies the exceptions that should be trapped when
the test is executed.  This is unimplemented. -}
data TrappedException = TrappedInexact | TrappedUnderflow | TrappedOverflow |
  TrappedDivisionByZero | TrappedInvalid deriving (Show, Eq)

instance Display (TrappedException) where
  display TrappedInexact = "x"
  display TrappedUnderflow = "u"
  display TrappedOverflow = "o"
  display TrappedDivisionByZero = "z"
  display TrappedInvalid = "i"

{- | 'Exception' specifies a type exception that the test should raise.
This is unimplemented. -}
data Exception = Inexact |
  ExtraordinaryUnderflow | InexactTinyUnderflow | TinyInexactUnderflow |
  Overflow | DivisionByZero | Invalid deriving (Show, Eq)

instance Display (Exception) where
  display Inexact = "x"
  display ExtraordinaryUnderflow = "u"
  display InexactTinyUnderflow = "v"
  display TinyInexactUnderflow = "w"
  display Overflow = "o"
  display DivisionByZero = "z"
  display Invalid = "i"
