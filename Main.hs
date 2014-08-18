module FPTest where

-- import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.Parsec.Number
import Numeric
import Control.Monad (liftM, ap)
import Test.QuickCheck hiding (output)
import Data.Fixed (divMod')
import Data.Char (toUpper)
import Data.Word (Word64, Word32)
import Unsafe.Coerce (unsafeCoerce)

-- TODO:: Text.Parsec.Number accepts only lower case "p"

{- From "Floating-Point Test-Suite for IEEE", IBM Labs in Haifa, FPgen team.
 - Contact: Merav Aharoni

https://www.research.ibm.com/haifa/projects/verification/fpgen/papers/ieee-test-suite-v2.pdf

-}


{-
Operation-general models - These models are not dependent on operation
semantics, and are therefore applicable to all floating-point operations.

Operation-specific models - Each of these models is suitable for one or more
specific floating-point operations, and targets special features of the
operation semantics.

TODO:: (Not yet used)
 -}
data OperationModel = OperationGeneral | OperationSpecific deriving (Show)

data EnableBit = EnableBit {
  inexactResult :: Bool,
  underflow :: Bool,
  overflow :: Bool,
  divideByZero :: Bool,
  invalidOperation :: Bool
  } deriving (Show)

data Format = BasicFormat BasicFormat |
  ComparisonFormat BasicFormat BasicFormat deriving (Eq)

instance Show (Format) where
  show (BasicFormat bf) = show bf
  show (ComparisonFormat bf1 bf2) = show bf1 ++ show bf2


data BasicFormat = Binary32 | Binary64 | Binary128 |
  Decimal32 | Decimal64 | Decimal128 deriving (Eq)

instance Show (BasicFormat) where
  show Binary32 = "b32"
  show Binary64 = "b64"
  show Binary128 = "b128"
  show Decimal32 = "d32"
  show Decimal64 = "d64"
  show Decimal128 = "d128"


data Operation = Add | Subtract | Multiply | Divide | FusedMultiplyAdd |
  SquareRoot | Remainder | RoundFloatToInteger |
  ConvertFloatToFloat | ConvertFloatToInteger | ConvertIntegerToFloat |
  ConvertDecimalToString | ConvertStringToDecimal |
  QuietComparison | SignallingComparison |
  Copy | Negate | Abs | CopySign | Scalb | Logb | NextAfter | Class |
  IsSigned | IsNormal | IsFinite | IsZero | IsSubNormal |
  IsInf | IsNaN | IsSignalling |
  MinNum | MaxNum | MinNumMag | MaxNumMag | SameQuantum | Quantize |
  NextUp | NextDown | Equivalent

instance Show (Operation) where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"
  show FusedMultiplyAdd = "*+"
  show SquareRoot = "V"
  show Remainder = "%"
  show RoundFloatToInteger = "rfi"
  show ConvertFloatToFloat = "cff"
  show ConvertFloatToInteger = "cfi"
  show ConvertIntegerToFloat = "cif"
  show ConvertDecimalToString = "cfd"
  show ConvertStringToDecimal = "cdf"
  show QuietComparison = "qC"
  show SignallingComparison = "sC"
  show Copy = "cp"
  show Negate = "~"
  show Abs = "A"
  show CopySign = "@"
  show Scalb = "S"
  show Logb = "L"
  show NextAfter = "Na"
  show Class = "?"
  show IsSigned = "?-"
  show IsNormal = "?n"
  show IsFinite = "?f"
  show IsZero = "?0"
  show IsSubNormal = "?s"
  show IsInf = "?i"
  show IsNaN = "?N"
  show IsSignalling = "?sN"
  show MinNum = "<C"
  show MaxNum = ">C"
  show MinNumMag = "<A"
  show MaxNumMag = ">A"
  show SameQuantum = "=quant"
  show Quantize = "quant"
  show NextUp = "Nu"
  show NextDown = "Nd"
  show Equivalent = "eq"

data RoundingMode = PositiveInfinity | NegativeInfinity | Zero | NearestEven |
  NearestAwayFromZero

instance Show (RoundingMode) where
  show PositiveInfinity = ">"
  show NegativeInfinity = "<"
  show Zero = "0"
  show NearestEven = "=0"
  show NearestAwayFromZero = "^0"


data TrappedException = TrappedInexact | TrappedUnderflow | TrappedOverflow |
  TrappedDivisionByZero | TrappedInvalid

instance Show (TrappedException) where
  show TrappedInexact = "x"
  show TrappedUnderflow = "u"
  show TrappedOverflow = "o"
  show TrappedDivisionByZero = "z"
  show TrappedInvalid = "i"
  showList [] = showString ""
  showList (x : xs) = shows x . shows xs


data Exception = Inexact |
  ExtraordinaryUnderflow | InexactTinyUnderflow | TinyInexactUnderflow |
  Overflow | DivisionByZero | Invalid

instance Show (Exception) where
  show Inexact = "x"
  show ExtraordinaryUnderflow = "u"
  show InexactTinyUnderflow = "v"
  show TinyInexactUnderflow = "w"
  show Overflow = "o"
  show DivisionByZero = "z"
  show Invalid = "i"
  showList [] = showString ""
  showList (x : xs) = shows x . shows xs

{- This type is for the parsed test cases
The operands are kept as strings so that
we can test different functions for parsing them.
TODO:: take the format out of the record and make it a parameter of
TestCase, so that we can write different eval instances. -}
data TestCase operationType = TestCase {
  format :: Format,
  operation :: Operation,
  roundingMode :: RoundingMode,
  trappedExceptions :: [TrappedException],
  inputs :: [operationType],
  output :: operationType,
  outputExceptions :: [Exception]
  }

instance Show operation => Show (TestCase operation) where
  show TestCase {
  format = f,
  operation = op,
  roundingMode = rm,
  trappedExceptions = te,
  inputs = ins,
  output = out,
  outputExceptions = oe} = show f ++ show op ++ " " ++ show rm ++ show te ++ " " ++ show ins ++ " -> " ++ show out ++ " " ++ show oe

type ParsedTestCase = TestCase String
type InterpretedTestCase = TestCase Float

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
outputSpec = operandSpec <|> (char '#' >> return "")

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
  (string "0x1" >> return "+1.0P0")

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


class (RealFloat a, Show a) => HasNaN a where
    signallingNaN :: a
    quietNaN :: a

instance HasNaN Double where
    signallingNaN = unsafeCoerce (0x7ff4000000000000 :: Word64)
    quietNaN = unsafeCoerce (0x7ff8000000000000 :: Word64)

instance HasNaN Float where
    signallingNaN = unsafeCoerce (0x7fa00000 :: Word32)
    quietNaN = unsafeCoerce (0x7fc00000 :: Word32)

boolNum :: RealFloat f => Bool -> f
boolNum True = 1.0
boolNum False = 0.0


{- TODO:: Could use CFloat, CDouble
TODO:: Consider endianness -}

eval :: ParsedTestCase -> String
eval TestCase {format = f,
  operation = op,
  roundingMode = rm,
  trappedExceptions = te,
  inputs = is,
  output = o,
  outputExceptions = oe} =
    case f of
      BasicFormat Binary32 ->
        case op of
          Add -> floatToHex (i1 + i2)
          Subtract -> floatToHex (i1 - i2)
          Multiply -> floatToHex (i1 * i2)
          Divide -> floatToHex (i1 / i2)
          SquareRoot -> floatToHex $ sqrt i1
          Remainder -> floatToHex r where (_, r) = i1 `divMod'` i2 -- i1 bound to Integer
          RoundFloatToInteger -> floatToHex $ fromIntegral $ round i1 -- defaults to Double
          ConvertFloatToFloat -> floatToHex i1 -- TODO::
          Negate -> floatToHex $ negate i1
          Abs -> floatToHex $ abs i1
          Logb -> floatToHex $ log i1
          IsZero -> floatToHex $ boolNum (i1 == 0.0)
          IsSubNormal -> floatToHex $ boolNum $ isDenormalized i1
          isSigned -> floatToHex $ boolNum $ i1 < 0.0
          IsNaN -> floatToHex $ boolNum $ isNaN i1
          isFinite -> floatToHex $ boolNum $ not $ isInfinite i1
          isInf -> floatToHex $ boolNum $ isInfinite i1
          _ -> "Unimplemented op: " ++ show op
        where
          i1 :: Float
          i1 = hexToFloat $ head is
          i2 :: Float
          i2 = hexToFloat $ head $ tail is

      BasicFormat Binary64 ->
        case op of
          Add -> floatToHex (i1 + i2)
          Subtract -> floatToHex (i1 - i2)
          Multiply -> floatToHex (i1 * i2)
          Divide -> floatToHex (i1 / i2)
          SquareRoot -> floatToHex $ sqrt i1
          Remainder -> floatToHex r where (_, r) = i1 `divMod'` i2 -- i1 bound to Integer
          RoundFloatToInteger -> floatToHex $ fromIntegral $ round i1 -- defaults to Double
          ConvertFloatToFloat -> floatToHex i1 -- TODO::
          Negate -> floatToHex $ negate i1
          Abs -> floatToHex $ abs i1
          Logb -> floatToHex $ log i1
          IsZero -> floatToHex $ boolNum (i1 == 0.0)
          IsSubNormal -> floatToHex $ boolNum $ isDenormalized i1
          isSigned -> floatToHex $ boolNum $ i1 < 0.0
          IsNaN -> floatToHex $ boolNum $ isNaN i1
          isFinite -> floatToHex $ boolNum $ not $ isInfinite i1
          isInf -> floatToHex $ boolNum $ isInfinite i1
          _ -> "Unimplemented op: " ++ show op
        where
          i1 :: Double
          i1 = hexToFloat $ head is
          i2 :: Double
          i2 = hexToFloat $ head $ tail is

      _ -> "Unimplemented format: " ++ show f


checkResult :: ParsedTestCase -> String
checkResult t@TestCase { output = o, outputExceptions = oe}
  | take (length "Unimplemented") et == "Unimplemented" = "."
  | strcmp et o = "Success!"
  | otherwise = eval t ++ "/=" ++ show o
  where et = eval t

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
  | s == "Q" = 0 / 0
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
floatToHex :: (RealFloat a, Show a) => a -> String
floatToHex x
  | isNaN x = "NaN"
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
      binaryDigitsToString ([0], n ) = "Zero"
      binaryDigitsToString ( 0 : denormalizedBinaryFraction, n) = "Denormalized"
      binaryDigitsToString _ = error "binaryDigitsToString"

      normalizedBinaryFractionToString :: ([Int], Int) -> String
      normalizedBinaryFractionToString (nbf, n) =
        binaryToHex nbf ++ "P" ++ exponentSign n ++ show n
          where
            exponentSign s
              | s < 0 = ""
              | otherwise = "+"

      binaryToHex :: [Int] -> String
      binaryToHex (a : b : c : d : es) =
        showHex (8 * a + 4 * b + 2 * c + d) (binaryToHex es)
      binaryToHex [a, b, c] = showHex (8 * a + 4 * b + 2 * c) ""
      binaryToHex [a, b] = showHex (8 * a + 4 * b) ""
      binaryToHex [a] = showHex (8 * a) ""
      binaryToHex [] = ""


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
                      print xs

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
        Right xs -> print (map checkResult xs)


check :: String
check =
  case parse testCaseSpec "" "b32?i =0 +1.000000P0 -> 0x0\nb32+ =0 i +0.000001P-126 +1.000000P-126 -> +1.000001P-126" of
  Left err -> show err
  Right t -> checkResult t


testFiles :: [String]
testFiles = map ("test_suite/" ++)
  [ "Basic-Types-Inputs.fptest"]


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
