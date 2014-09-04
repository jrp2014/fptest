{- |
Module      : FPMain
Description : "FPMain" executes IBM floating point test cases either directly,
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
module Main where

import FPRun

{-

TODO :: This initial version uses only basic Haskell, and so does not manipulate the floating point environment
for handling floating point exceptions, rounding mode, etc

-}


{- main = do
  {- quickCheck (prop_FloatHex :: Float -> Bool)
  quickCheck (prop_FloatHex :: Double -> Bool)
  evalTests -}
  translateTests
-}


main :: IO ()
main =
  {- quickCheck (prop_FloatHex :: Float -> Bool)
  quickCheck (prop_FloatHex :: Double -> Bool)
  evalTests -}
  translateTests testFiles


testFiles :: [FilePath]
testFiles = map ("test_suite/" ++)
  [ "Basic-Types-Intermediate.fptest"]

saveTestFiles :: [FilePath]
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
  "Rounding.fptest",
  "Sticky-Bit-Calculation.fptest",
  "Underflow.fptest",
  "Vicinity-Of-Rounding-Boundaries.fptest"]
