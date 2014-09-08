module Main where

import FPRun
import qualified Test.QuickCheck as Q


-- | == Checks

-- | QuickCheck test
prop_FloatHex :: (RealFloat a, HasNaN a, Show a) => a -> Q.Property
prop_FloatHex f = hexToFloat (floatToHex f) Q.=== f


prop_test1f :: Float -> Q.Property
prop_test1f = prop_FloatHex
prop_test1d :: Double -> Q.Property
prop_test1d = prop_FloatHex

rf, rd :: IO()
rf = Q.quickCheckWith Q.stdArgs {Q.maxSuccess = 5000} prop_test1f
rd = Q.quickCheckWith Q.stdArgs {Q.maxSuccess = 5000} prop_test1d


-- | 'check2' parse and check a couple of test cases
-- check2 :: IO ()
-- check2 = putStrLn $ unlines $
--   case parse (endBy1 testCaseSpec eol) ""
--     "b32?i =0 +1.000000P0 -> 0x0\r\nb32+ =0 i +0.000001P-126 +1.000000P-126 -> +1.000002P-126\r\nb32V =0 -1.7FFFFFP127 -> Q i" of
--   Left err -> [show err]
--   Right t -> fmap checkResult t

main :: IO()
main = do
   rf
   rd


  -- NB: These tests do not use the standard floating point hex listeral representation but
  -- an IBM version that right justifies the 23 bits of the significand into the 24 bits
  -- needed to pruce a hex representation.

   Q.quickCheck $ floatToHex (2**(-149) :: Double) Q.=== "+1.0000000000000P-149"
   Q.quickCheck $ floatToHex (2**(-149) :: Float) Q.=== "+0.000001P-126"
     -- and so not  "+0.000002P-126" as it would be in the conventional hex literal notation

  
   Q.quickCheck $ floatToHex (2.0 :: Double) Q.=== "+1.0000000000000P+1"
   Q.quickCheck $ floatToHex (2.0 :: Float) Q.=== "+1.000000P+1"

   Q.quickCheck $ floatToHex (3.0 :: Double) Q.=== "+1.8000000000000P+1"
   Q.quickCheck $ floatToHex (3.0 :: Float) Q.=== "+1.800000P+1"
  
   Q.quickCheck $ floatToHex (2.0**8 :: Double) Q.=== "+1.0000000000000P+8"
   Q.quickCheck $ floatToHex (2.0**8 :: Float) Q.=== "+1.000000P+8"


   Q.quickCheck $ floatToHex (2.0**(-6) :: Double) Q.=== "+1.0000000000000P-6"
   Q.quickCheck $ floatToHex (2.0**(-6) :: Float) Q.=== "+1.000000P-6"

   Q.quickCheck $ floatToHex (0.857421875 :: Double) Q.=== "+1.B700000000000P-1"
   Q.quickCheck $ floatToHex (0.857421875 :: Float) Q.===  "+1.B70000P-1"

   Q.quickCheck $ floatToHex (3.1415926 :: Double) Q.=== "+1.921FB4D12D84AP+1"
   Q.quickCheck $ floatToHex (3.1415926 :: Float) Q.===  "+1.921FB4P+1" -- rounding?

   Q.quickCheck $ floatToHex (0.1 :: Double) Q.=== "+1.999999999999AP-4"
   Q.quickCheck $ floatToHex (0.1 :: Float) Q.===  "+1.99999AP-4"

   Q.quickCheck $ (hexToFloat "+1.999999999999AP-4" :: Double) Q.=== 0.1
   Q.quickCheck $ (hexToFloat "+1.99999AP-4" :: Float) Q.=== 0.1

   Q.quickCheck $ floatToHex ( hexToFloat "cc.ccccccccccdp-11" :: Double ) Q.=== "+1.999999999999AP-4"
   Q.quickCheck $ floatToHex ( hexToFloat "cc.ccccccccccdp-11" :: Float ) Q.=== "+1.99999AP-4"

   Q.quickCheck $ floatToHex ( hexToFloat "3.3333333333334p-5" :: Double ) Q.=== "+1.999999999999AP-4"
   Q.quickCheck $ floatToHex ( hexToFloat "3.3333333333334p-5" :: Float ) Q.=== "+1.99999AP-4"


   -- Max normal
   Q.quickCheck $ floatToHex (((2-2**(-52))*2**1023) :: Double) Q.=== "+1.FFFFFFFFFFFFFP+1023"
   Q.quickCheck $ floatToHex (((2-2**(-23))*2**127) :: Float) Q.=== "+1.7FFFFFP+127"

   -- Min normal
   Q.quickCheck $ (hexToFloat "0.0000000000001P-1022" :: Double) Q.=== 5.0e-324
   Q.quickCheck $ floatToHex (2**(-1022) :: Double) Q.=== "+1.0000000000000P-1022"
   Q.quickCheck $ (hexToFloat "+1.000000P-126" :: Float) Q.=== 1.1754944e-38
   Q.quickCheck $ floatToHex (2**(-126) :: Float) Q.=== "+1.000000P-126"

   -- Min subnormal
   Q.quickCheck $ floatToHex (2**(-1074) :: Double) Q.=== "+0.00000000000001P-1022"
   Q.quickCheck $ floatToHex (hexToFloat "1.0P-1074" :: Double) Q.=== "+0.00000000000001P-1022"
   

   -- Max subnormal 
   Q.quickCheck $ floatToHex (2**(-1022) - (2**(-1022-52)) :: Double) Q.=== "+0.7FFFFFFFFFFFF1P-1022"
   Q.quickCheck $ floatToHex (2**(-126) - (2**(-126-23)) :: Float) Q.=== "+0.7FFFFFP-126"
   Q.quickCheck $ (hexToFloat "+0.7FFFFFP-126" :: Float) Q.=== (2**(-126) - (2**(-126-23))) 
