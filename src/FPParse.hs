{- |
Module      : FPParse
Description : "FPParse" parses IBM floating point test cases into a list of 'ParsedTestCase'.
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


module FPParse where

import           FPTypes

import           Text.ParserCombinators.Parsec

-- |
-- = Test case specification parsing


{- | 'testCaseFile' parses a 'String' (typically a test case specification
file) into a list of 'ParsedTestCase' using ParSec -}
testCaseFile :: GenParser Char st [ParsedTestCase]
testCaseFile =
  do
    _ <- testCaseFileHeader -- discarded
    endBy1 testCaseSpec eol -- Expect at least one test case

-- | 'eol' is a newine or end of file.
eol :: GenParser Char st Char
eol = newline <|> (eof >> return '\n')

{- | 'testCaseFileHeader' discards the first 4 (copyright)
lines of a test case file -}
testCaseFileHeader :: GenParser Char st [()]
testCaseFileHeader = count 4 testCaseFileHeaderLine
  where
    testCaseFileHeaderLine =
      do
        skipMany (noneOf "\n")
        _ <- eol
        return ()


{- | 'testCaseSpec' parses a test case, as set out in Chapter 4 of
<https://www.research.ibm.com/haifa/projects/verification/fpgen/papers/ieee-test-suite-v2.pdf
Floating-Point Test-Suite for IEEE> -}
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
     ComparisonFormat f1 <$> basicFormatSpec) <|>
   do
     BasicFormat <$> basicFormatSpec

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

{- | 'binaryFloatingPoint' parses either a hex floating point literal, or
one of the NaNs, +/-Infinity or +/-Zero -}
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

{- 'boolean' parses 0x0 or 0x1, returning a floating point notation
reresentation of the value -}
boolean :: GenParser Char st String
boolean =
  (try (string "0x0") >> return "+Zero") <|>
  (string "0x1" >> return "+1.000000P+0")

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


-- |
-- = Helper functions for parser testing


-- | 'parseTestCaseFile' applied to a file name string
-- parses a test case file using the 'testCaseFile'
-- parser.  The result is either a 'Left' error message or
-- a 'Right' 'ParsedTestCase' list.
parseTestCaseFile :: FilePath -> Either ParseError [ParsedTestCase]
parseTestCaseFile = parse testCaseFile "parseTestCaseFile"


-- | 'parseTests' takes a list of test case specification file names
-- and prints a list of file name and parsed test cases list
parseTests :: [FilePath] -> IO ()
parseTests = mapM_ parseTestCases
  where
    -- Parse test cases in a file using the tesCaseFile parser
    parseTestCases :: FilePath -> IO ()
    parseTestCases fileName = do
      result <- parseFromFile testCaseFile fileName
      print fileName
      case result of
        Left err -> do
                      print err
                      error "failed"
        Right xs -> printd xs




