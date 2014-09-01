-- {-# LANGUAGE NoMonomorphismRestriction #-}
module FPTest where

-- import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.Parsec.Number
-- import ParsecNumber
import Numeric
import Control.Monad (liftM, ap)
import Test.QuickCheck hiding (output)
import Data.Fixed (divMod')
import Data.Char (toUpper)
import Data.Either (rights)
import Data.List (intercalate)
import Data.Word (Word64, Word32)
import Unsafe.Coerce (unsafeCoerce)

{- From "Floating-Point Test-Suite for IEEE", IBM Labs in Haifa, FPgen team.
 - Contact: Merav Aharoni

https://www.research.ibm.com/haifa/projects/verification/fpgen/papers/ieee-test-suite-v2.pdf


TODO :: This initial version uses only basic Haskell, and so does not manipulate the floating point environment
for handling floating point exceptions, rounding mode, etc

-}


{-
Operation-general models - These models are not dependent on operation
semantics, and are therefore applicable to all floating-point operations.

Operation-specific models - Each of these models is suitable for one or more
specific floating-point operations, and targets special features of the
operation semantics.

TODO:: (Not yet used)
 -}

class Show a => Display a where
  display :: a -> String
  display = show

  displayList :: [a] -> String
  displayList list = unwords (map display list)

  printd :: a -> IO ()
  printd = putStrLn . display

instance Display a => Display [a] where
  display = displayList

instance Display Char where
  displayList = id


data OperationModel = OperationGeneral | OperationSpecific deriving (Show)

data EnableBit = EnableBit {
  inexactResult :: Bool,
  underflow :: Bool,
  overflow :: Bool,
  divideByZero :: Bool,
  invalidOperation :: Bool
  } deriving (Show)

data Format = BasicFormat BasicFormat |
  ComparisonFormat BasicFormat BasicFormat deriving (Show, Eq)

instance Display (Format) where
  display (BasicFormat bf) = display bf
  display (ComparisonFormat bf1 bf2) = display bf1 ++ display bf2


-- only b32 (Float) and b64 (Double) tests are implemented
data BasicFormat = Binary32 | Binary64 | Binary128 |
  Decimal32 | Decimal64 | Decimal128 deriving (Show, Eq)

instance Display (BasicFormat) where
  display Binary32 = "b32"
  display Binary64 = "b64"
  display Binary128 = "b128"
  display Decimal32 = "d32"
  display Decimal64 = "d64"
  display Decimal128 = "d128"


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

data RoundingMode = PositiveInfinity | NegativeInfinity | Zero | NearestEven |
  NearestAwayFromZero deriving (Show)

instance Display (RoundingMode) where
  display PositiveInfinity = ">"
  display NegativeInfinity = "<"
  display Zero = "0"
  display NearestEven = "=0"
  display NearestAwayFromZero = "^0"


data TrappedException = TrappedInexact | TrappedUnderflow | TrappedOverflow |
  TrappedDivisionByZero | TrappedInvalid deriving (Show, Eq)

instance Display (TrappedException) where
  display TrappedInexact = "x"
  display TrappedUnderflow = "u"
  display TrappedOverflow = "o"
  display TrappedDivisionByZero = "z"
  display TrappedInvalid = "i"

data Exception = Inexact |
  ExtraordinaryUnderflow | InexactTinyUnderflow | TinyInexactUnderflow |
  Overflow | DivisionByZero | Invalid deriving (Show)

instance Display (Exception) where
  display Inexact = "x"
  display ExtraordinaryUnderflow = "u"
  display InexactTinyUnderflow = "v"
  display TinyInexactUnderflow = "w"
  display Overflow = "o"
  display DivisionByZero = "z"
  display Invalid = "i"

{- This type is for the parsed test cases
The operands are kept as strings so that
we can test different functions for parsing them.
-}
data TestCase operationType = TestCase {
  format :: Format,
  operation :: Operation,
  roundingMode :: RoundingMode,
  trappedExceptions :: [TrappedException],
  inputs :: [operationType],
  output :: operationType,
  outputExceptions :: [Exception]
  } deriving (Show)

type ParsedTestCase = TestCase String


instance (Display a, Show a) => Display (TestCase a) where
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


-- -------------------- Test Case File Parser ----------------------------------
testCaseFile :: GenParser Char st [ParsedTestCase]
testCaseFile =
  do
    _ <- testCaseFileHeader -- discarded
    endBy1 testCaseSpec eol -- Expect at least one test case

eol :: GenParser Char st Char
eol = newline <|> (eof >> return '\n')

-- Ignore first 4 lines of test case file
testCaseFileHeader :: GenParser Char st [()]
testCaseFileHeader = count 4 testCaseFileHeaderLine
  where
    testCaseFileHeaderLine =
      do
        skipMany (noneOf "\n")
        _ <- eol
        return ()


-- From Chapter 4
testCaseSpec :: GenParser Char st ParsedTestCase
testCaseSpec =
  do
    b <- formatSpec
    o <- operationSpec
    _ <- space
    r <- roundingModeSpec
    _ <- space
    t <- option [] (do
                      t' <- trappedExceptionSpec
                      _ <- space
                      return t')
    i <- inputSpec
    _ <- string "-> "
    p <- outputSpec
    x <- option [] (do
                      _ <- space
                      outputExceptionsSpec)

    return TestCase {
      format = b,
      operation = o,
      roundingMode = r,
      trappedExceptions = t,
      inputs = i,
      output = p,
      outputExceptions = x
    }


formatSpec :: GenParser Char st Format
formatSpec =
  try (do
     f1 <- basicFormatSpec
     f2 <- basicFormatSpec
     return (ComparisonFormat f1 f2)) <|>
   do
     f <- basicFormatSpec
     return (BasicFormat f)

basicFormatSpec :: GenParser Char st BasicFormat
basicFormatSpec = binarySpec <|> decimalSpec

binarySpec :: GenParser Char st BasicFormat
binarySpec = char 'b' >> binaryWidth

binaryWidth :: GenParser Char st BasicFormat
binaryWidth =
  (string "32" >> return Binary32) <|>
  (string "64" >> return Binary64) <|>
  (string "128" >> return Binary128)

decimalSpec :: GenParser Char st BasicFormat
decimalSpec = char 'd' >> decimalWidth

decimalWidth :: GenParser Char st BasicFormat
decimalWidth =
  (string "32" >> return Decimal32) <|>
  (string "64" >> return Decimal64) <|>
  (string "128" >> return Decimal128)


operationSpec :: GenParser Char st Operation
operationSpec =
  (char '+' >> return Add) <|>
  (char '-' >> return Subtract) <|>
  (try (string "*+") >> return FusedMultiplyAdd) <|>
  (char '*' >> return Multiply) <|>
  (char '/' >> return Divide) <|>
  (char 'V' >> return SquareRoot) <|>
  (char '%' >> return Remainder) <|>
  (string "rfi" >> return RoundFloatToInteger) <|>
  (try (string "cff") >> return ConvertFloatToFloat) <|>
  (try (string "cfi") >> return ConvertFloatToInteger) <|>
  (try (string "cif") >> return ConvertIntegerToFloat) <|>
  (try (string "cfd") >> return ConvertDecimalToString) <|>
  (try (string "cdf") >> return ConvertStringToDecimal) <|>
  (try (string "qC") >> return QuietComparison) <|>
  (string "sC" >> return SignallingComparison) <|>
  (string "cp" >> return Copy) <|>
  (char '~' >> return Negate) <|>
  (char 'A' >> return Abs) <|>
  (char '@' >> return CopySign) <|>
  (char 'S' >> return Scalb) <|>
  (char 'L' >> return Logb) <|>
  (try (string "Na") >> return NextAfter) <|>
  (try (string "?-") >> return IsSigned) <|>
  (try (string "?n") >> return IsNormal) <|>
  (try (string "?f") >> return IsFinite) <|>
  (try (string "?0") >> return IsZero) <|>
  (try (string "?sN") >> return IsSignalling) <|>
  (try (string "?s") >> return IsSubNormal) <|>
  (try (string "?i") >> return IsInf) <|>
  (try (string "?N") >> return IsNaN) <|>
  (try (char '?') >> return Class) <|>
  (try (string "<C") >> return MinNum) <|>
  (try (string ">C") >> return MaxNum) <|>
  (string "<A" >> return MinNumMag) <|>
  (string ">A" >> return MaxNumMag) <|>
  (string "=quant" >> return SameQuantum) <|>
  (string "quantize" >> return Quantize) <|>
  (try (string "Nu") >> return NextUp) <|>
  (string "Nd" >> return NextDown) <|>
  (string "eq" >> return Equivalent)

roundingModeSpec :: GenParser Char st RoundingMode
roundingModeSpec =
  (char '>' >> return PositiveInfinity) <|>
  (char '<' >> return NegativeInfinity) <|>
  (char '0' >> return Zero) <|>
  (try (string "=0") >> return NearestEven) <|>
  (string "=^" >> return NearestAwayFromZero)

trappedExceptionSpec :: GenParser Char st [TrappedException]
trappedExceptionSpec = many (
  (char 'x' >> return TrappedInexact) <|>
  (char 'u' >> return TrappedUnderflow) <|>
  (char 'o' >> return TrappedOverflow) <|>
  (char 'z' >> return TrappedDivisionByZero) <|>
  (char 'i' >> return TrappedInvalid)
  )

operandSpec :: GenParser Char st String
operandSpec =
  try binaryFloatingPoint <|>
  decimalFloatingPoint <|>
  dPDFormat <|>
  integer <|>
  boolean <|>
  returnClass

inputSpec :: GenParser Char st [String]
inputSpec = operandSpec `sepEndBy1` char ' '

outputSpec :: GenParser Char st String
outputSpec = operandSpec <|> (char '#' >> return "#")

-- This is really hex
binaryFloatingPoint :: GenParser Char st String
binaryFloatingPoint =
  try (do
       sn <- signChar
       sd <- hexSignificand
       _ <- char 'P'
       e <- unbiasedExponent
       return (sn : sd ++ 'P' : e)) <|>
  string "S" <|> string "Q" <|>
  try (do
      sn <- signChar
      _ <- string "Inf" <|> string "inf"
      return (sn : "Inf")) <|>
  (do
    sn <- signChar
    _ <- string "Zero"
    return (sn : "Zero"))


decimalFloatingPoint :: GenParser Char st String
decimalFloatingPoint =
  try (do
       sn <- signChar
       sd <- decSignificand
       _ <- oneOf "Ee"
       e <- unbiasedExponent
       return (sn : sd ++ 'E' : e)) <|>
  string "S" <|> string "Q" <|>
  try (do
      sn <- signChar
      i <- string "Inf"
      return (sn : i))

signChar :: GenParser Char st Char
signChar = oneOf "+-"

hexSignificand :: GenParser Char st String
hexSignificand = many1 (hexDigit <|> char '.')

decSignificand :: GenParser Char st String
decSignificand = many1 (digit <|> char '.')

unbiasedExponent :: GenParser Char st String
unbiasedExponent = do
  s <- option '+' signChar
  d <- many1 digit
  return (s : d)


dPDFormat :: GenParser Char st String
dPDFormat = do
  _ <- string "DPD_"
  h <- hexSignificand
  return ("DPD_" ++ h)

integer :: GenParser Char st String
integer = try
  (do
  s <- signChar
  i <- many1 digit
  return (s : i))

-- Return a form that will match floatToHex
boolean :: GenParser Char st String
boolean =
  (try (string "0x0") >> return "+Zero") <|>
  (string "0x1" >> return "1.P+0")

returnClass :: GenParser Char st String
returnClass =
    string "sNaN" <|>
    string "qNaN" <|>
    try (string "-Inf") <|>
    try (string "-normal") <|>
    try (string "-subnormal") <|>
    try (string "-0") <|>
    try (string "+0") <|>
    try (string "+subnormal") <|>
    try (string "+normal") <|>
    string "+Inf"


outputExceptionsSpec :: GenParser Char st [Exception]
outputExceptionsSpec = many (
  (char 'x' >> return Inexact) <|>
  (char 'u' >> return ExtraordinaryUnderflow) <|>
  (char 'v' >> return InexactTinyUnderflow) <|>
  (char 'w' >> return TinyInexactUnderflow) <|>
  (char 'o' >> return Overflow) <|>
  (char 'z' >> return DivisionByZero) <|>
  (char 'i' >> return Invalid)
  )


-- -- Test case execution ------------------------------------------------------

data TestResult = Pass | Fail | NotRun

-- runTest :: TestCase -> TestResult


{- interpretTest :: ParsedTestCase -> InterpretedTestCase
interpretTest t =
interpretedTestCase {
format = format t,
operation = operation t,
roundingMode = roundingMode t,
trappedExceptions = trappedExceptions t,
inputs = inputs t,
output = output t,
outputExceptions = outputExceptions t
}
 -}


-- Placeholder
type Quad = Double


 {- Some example code that allows NaNs to signal, from
 http://stackoverflow.com/questions/21344139/ieee-floating-point-signalling-nan-snan-in-haskell
 No good for us here, as haskell does not handle signalling at all -}

{- {-# LANGUAGE ForeignFunctionInterface #-}
import Data.Word (Word64, Word32)
import Unsafe.Coerce
import Foreign
import Foreign.C.Types
foreign import ccall "fenv.h feenableexcept" -- GNU extension
enableexcept :: CInt -> IO () -}

class (RealFloat a, Show a) => HasNaN a where
    signallingNaN :: a
    quietNaN :: a

instance HasNaN Double where
    signallingNaN = unsafeCoerce (0x7ff4000000000000 :: Word64)
    quietNaN = unsafeCoerce (0x7ff8000000000000 :: Word64)

instance HasNaN Float where
    signallingNaN = unsafeCoerce (0x7fa00000 :: Word32)
    quietNaN = unsafeCoerce (0x7fc00000 :: Word32)

{- main = do
enableexcept 1 -- FE_INVALID in my system
print $ show $ 1 + (quietNaN :: Float) -- works
print $ show $ 1 + (signalingNaN :: Float) -- fails
 -}


boolNum :: RealFloat f => Bool -> f
boolNum True = 1.0
boolNum False = 0.0

type ErrMsg = String
type Output a = Either ErrMsg a

execute :: (RealFloat a, HasNaN a) => ParsedTestCase -> Output a
execute t@TestCase { format = f,
operation = op,
  {- roundingMode = rm,
  trappedExceptions = te, -}
  inputs = is,
  output = o
  -- outputExceptions = oe
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
      -- | x is signalling NaN = 0.0
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

              expected = ot ++ " ~==? "

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

-- Check the result of runnning the test directly
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
          Left err -> err
          Right result
            | o == "#" -> "No output expected: " ++ show result
            | floatToHex result == o -> "Success!"
            | otherwise -> floatToHex result ++ " (" ++ show result ++ ") /= " ++ display o


-- Translate the test into a diagnostic String or an HUnit test
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


-- Case insensitive string comparison
strcmp :: String -> String -> Bool
strcmp [] [] = True
strcmp s1 s2 = case (s1, s2) of
  (s11 : ss1, s21 : ss2)
    | toUpper s11 == toUpper s21 -> strcmp ss1 ss2
    | otherwise -> False
  _ -> False


-- -- Helper functions to work with Hex floating point format

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


{- TODO:: handle denormalized case
TODO:: refactor this in terms of encodeFloat, signCharificand, etc -}

-- The hex format of float
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


-- Tests
prop_FloatHex :: (RealFloat a, HasNaN a, Show a) => a -> Bool
prop_FloatHex f = hexToFloat (floatToHex f) == f

-- -- Helper functions for testing the test case file parser -------------------

parseTestCaseFile :: String -> Either ParseError [ParsedTestCase]
parseTestCaseFile = parse testCaseFile "parseTestCaseFile"

parseTests :: IO ()
parseTests = mapM_ parseTestCases testFiles
  where
    -- Parse test cases in a file
    parseTestCases :: String -> IO ()
    parseTestCases f = do
      result <- parseFromFile testCaseFile f
      case result of
        Left err -> do
                      print err
                      error "failed"
        Right xs -> do
                      print f
                      printd xs

evalTests :: IO ()
evalTests = mapM_ evalTestCases testFiles
  where
    evalTestCases :: String -> IO ()
    evalTestCases f = do
      result <- parseFromFile testCaseFile f
      case result of
        Left err -> do
                      print err
                      error "Test case file failed to parse"
        -- Right xs -> mapM_ print (map checkResult xs)
        Right xs -> putStrLn $ unlines (map checkResult xs)


translateTests :: IO ()
translateTests = mapM_ translateTestCases testFiles
  where
    translateTestCases :: String -> IO ()
    translateTestCases f = do
      result <- parseFromFile testCaseFile f
      case result of
        Left err -> do
                      print err
                      error "Test case file failed to parse"
        Right xs -> do
          putStrLn "module FPTestTest where\n\n"
          putStrLn "import Test.HUnit\n"
          putStrLn "type D = Double"
          putStrLn "type F = Float\n"
          putStrLn "boolNum :: RealFloat a => Bool -> a"
          putStrLn "boolNum x"
          putStrLn "  | x = 1"
          putStrLn "  | otherwise = 0\n"

          putStrLn "-- Version that treats NaN == NaN"
          putStrLn "assertEqual' :: (Eq a, Show a, RealFloat a) => String -> a -> a -> Assertion"
          putStrLn "assertEqual' preface expected actual ="
          putStrLn " if (isNaN actual && isNaN expected) || (actual == expected)"
          putStrLn " then return () else assertFailure msg"
          putStrLn "    where msg = (if null preface then \"\" else preface ++ \"\\n\") ++"
          putStrLn "                \"expected: \" ++ show expected ++ \"\\n but got: \" ++ show actual"
          putStrLn ""
          putStrLn "infix 1 @==?, @?==, ~==?, ~?=="
          putStrLn ""
          putStrLn "(@==?) :: (Eq a, Show a, RealFloat a) => a -> a -> Assertion"
          putStrLn "expected @==? actual = assertEqual' \"\" expected actual"
          putStrLn ""
          putStrLn "(@?==) :: (Eq a, Show a, RealFloat a) => a -> a -> Assertion"
          putStrLn "actual @?== expected = assertEqual' \"\" expected actual"
          putStrLn ""
          putStrLn "(~==?) :: (Eq a, Show a, RealFloat a) => a -> a -> Test"
          putStrLn "expected ~==? actual = TestCase (expected @==? actual)"
          putStrLn ""
          putStrLn "(~?==) :: (Eq a, Show a, RealFloat a) => a -> a -> Test"
          putStrLn "actual ~?== expected = TestCase (actual @?== expected)"
          putStrLn ""

          putStrLn "main :: IO Counts"
          putStrLn "main = runTestTT tests"

          putStr $ "tests = " ++ show f ++ " ~: [\n  "
          putStr $ intercalate ",\n  " (rights (map translateResult xs))
          putStrLn "]"


main :: IO ()
main = do
  {- quickCheck (prop_FloatHex :: Float -> Bool)
  quickCheck (prop_FloatHex :: Double -> Bool)
  evalTests -}
  translateTests

check :: String
check =
  case parse testCaseSpec "" "b32?i =0 +1.000000P0 -> 0x0\nb32+ =0 i +0.000001P-126 +1.000000P-126 -> +1.000001P-126" of
  Left err -> show err
  Right t -> checkResult t


testFiles :: [String]
testFiles = map ("test_suite/" ++)
  [ "Basic-Types-Intermediate.fptest"]

saveTestFiles :: [String]
saveTestFiles =
  ["Add-Cancellation-And-Subnorm-Result.fptest",
  "Add-Cancellation.fptest",
  "Add-Shift-And-Special-Significands.fptest",
  "Add-Shift.fptest",
  "Basic-Types-Inputs.fptest",
  "Basic-Types-Intermediate.fptest",
  "Compare-Different-Input-Field-Relations.fptest",
  "Corner-Rounding.fptest",
  "Decimal-Basic-Types-Inputs.fptest",
  "Decimal-Basic-Types-Intermediate.fptest",
  "Decimal-Clamping.fptest",
  "Decimal-Mul-Trailing-Zeros.fptest",
  "Decimal-Overflow.fptest",
  "Decimal-Rounding.fptest",
  "Decimal-Trailing-And-Leading-Zeros-Input.fptest",
  "Decimal-Trailing-And-Leading-Zeros-Result.fptest",
  "Decimal-Underflow.fptest",
  "Divide-Divide-By-Zero-Exception.fptest",
  "Divide-Trailing-Zeros.fptest",
  "Hamming-Distance.fptest",
  "Input-Special-Significand.fptest",
  "MultiplyAdd-Cancellation-And-Subnorm-Result.fptest",
  "MultiplyAdd-Cancellation.fptest",
  "MultiplyAdd-Shift-And-Special-Significands.fptest",
  "MultiplyAdd-Shift.fptest",
  "MultiplyAdd-Special-Events-Inexact.fptest",
  "MultiplyAdd-Special-Events-Overflow.fptest",
  "MultiplyAdd-Special-Events-Underflow.fptest",
  "Overflow.fptest",
  "Rounding.fptest",
  "Sticky-Bit-Calculation.fptest",
  "Underflow.fptest",
  "Vicinity-Of-Rounding-Boundaries.fptest"]
