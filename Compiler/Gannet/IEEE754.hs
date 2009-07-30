-- | Encoding 64-bit IEEE floating-point numbers to 64-bit unsigned integer
module Gannet.IEEE754(
	encodeIEEE754,
	encodeIEEE754_32,
	encodeIEEE754_64
) where

import Data.Bits
import Data.Word
-- import Data.Char

{-
This is a Double-to-bytes conversion
- assume x is Double, i.e. 64-bit IEEE 754
- Forget NaN, infinity and -0
- Haskell's decodeFloat x returns (m,n) where x = m*2**n
- For denormalized doubles, we shift to the right; the exp is zero
- Unfortunately Haskell's shiftL works only on 32-bit Int's
-}
encodeIEEE754 :: Double -> Integer -- [Word8]		
encodeIEEE754 = encodeIEEE754_64


encodeIEEE754_64 :: Double -> Integer -- [Word8]		
encodeIEEE754_64 x
	| x == 0.0 = 0
	| otherwise =
		let
			signbit
				| x<0.0 = shiftL 1 63 
				| otherwise = 0
			xn
				| x<0 = (-x)
				| otherwise = x
			(m,n)=decodeFloat xn
			m1
				| m==0 = 0 
				| otherwise = m - pow2(52) -- (shiftL 1 52) 
			n1 
				| n>0 = (-n)
				| otherwise = n
			e1 = 1075 + n -- 1023 + 52; 1023=2**(11-1)-1 : exp is biased 
			fltw
				| isDenormalized x =	 (shiftR m (1-e1)) + signbit
				| otherwise =  m1 + (toInteger e1)*pow2(52) + signbit -- m1 +			
		in
			fltw	
			
encodeIEEE754_32 :: Float -> Integer
encodeIEEE754_32 x
	| x == 0.0 = 0
	| otherwise =
		let
			signbit
				| x<0.0 = shiftL 1 31
				| otherwise = 0
			xn
				| x<0 = (-x)
				| otherwise = x
			(m,n)=decodeFloat xn
			m1
				| m==0 = 0 
				| otherwise = m - pow2(23) -- (shiftL 1 23) 
			n1 
				| n>0 = (-n)
				| otherwise = n
			e1 = 150 + n -- 127 + 23; 127=2**(8-1)-1 (exp bias)
			fltw
				| isDenormalized x =	 (shiftR m (1-e1)) + signbit
				| otherwise =  m1 + (toInteger e1)*pow2(23) + signbit -- m1 +			
		in
			fltw			

pow2 :: Integer -> Integer
pow2 0 = 1
pow2 n = 2*(pow2 (n-1))

	
