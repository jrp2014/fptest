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

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO

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


data Action = Evaluate | Translate FilePath

data Options = Options {
    optAction :: [FilePath] -> IO String,
    optOutputToFile :: String -> IO ()
  }


defaultOptions :: Options
defaultOptions = Options {
    optAction = evalTestFiles,
    optOutputToFile = putStr
  }

options :: [OptDescr (Options -> IO Options)]
options = [
  Option "e" ["evaluate"]
    (NoArg
      (\ opt -> return opt { optAction = evalTestFiles }))
    "Evaluate test script file",

  Option "t" ["translate"]
    (NoArg
      (\ opt -> return opt { optAction = translateTestFiles }))
    "Translate test script file into a Haskell script",

  Option "o" ["output"]
    (ReqArg
      (\ arg opt -> return opt { optOutputToFile = writeFile arg })
      "FILE")
      "Output filename",

  Option "h" ["help"]
    (NoArg
      (\ _ -> do
        msg <- usage
        hPutStrLn stderr msg
        exitSuccess))
    "Show help"
  ]


usage :: IO String
usage = do
  prg <- getProgName
  return $ "Usage: " ++ usageInfo prg options

{- parseArgs :: IO Options
parseArgs = do
argv <- getArgs
progName <- getProgName
let header = "Usage: " ++ progName ++ " [OPTION...] FILE1.fptest FILE2.fptest ..."
let helpMessage = usageInfo header (options "")
case getOpt RequireOrder (options helpMessage) argv of
(opts, [], []) -> foldlM (flip id) defaultOptions opts
(_, _, errs) -> ioError (userError (concat errs ++ helpMessage))
 -}


main :: IO ()
main = do
  {- quickCheck (prop_FloatHex :: Float -> Bool)
  quickCheck (prop_FloatHex :: Double -> Bool)
  evalTests -}

  -- Read the arguments
  args <- getArgs

  case getOpt RequireOrder options args of
    (actions, scripts, []) -> do {- should check that there is only one action,
                                 rather than running the last one -}
                                opts <- foldl (>>=) (return defaultOptions) actions

                                -- Pull out the action and output action
                                let Options { optAction = action,
                                  optOutputToFile = output } = opts

                                {- feed the results of applying the action to the
                                scripts to output -}
                                action scripts >>= output

                                exitSuccess

    (_, [], _) -> do
                      hPutStrLn stderr "At least one test script argument required"
                      exitFailure

    (_, _, errs) -> do
                      u <- usage
                      hPutStrLn stderr (concat errs ++ u)

                      exitFailure


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
