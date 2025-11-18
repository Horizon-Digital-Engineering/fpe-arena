{-|
Module      : FF3.Core  
Description : Pure functional FF3 Format Preserving Encryption core algorithm
Copyright   : (c) Horizon Digital Engineering, 2025
License     : BUSL-1.1
Maintainer  : engineering@horizondigital.dev
Stability   : experimental

FF3 Core - Pure cryptographic implementation using functional programming paradigms.
This module implements FF3 FPE using immutable data structures, pure functions,
and referential transparency. No side effects, no mutable state.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module FF3.Core
  ( -- * Core Types
    FF3Cipher(..)
  , FF3Error(..)
    -- * Cipher Creation  
  , ff3Cipher
    -- * Core Operations
  , ff3Encrypt
  , ff3Decrypt
  , encryptDigits
  , decryptDigits
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Bits (xor)
import Data.List (foldl')
import Control.Monad (when)
import Crypto.Cipher.AES (AES128, AES192, AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..))
import Crypto.Error (CryptoFailable(..), CryptoError)

-- | FF3 Cipher instance - pure immutable data structure
-- Contains all necessary state for encryption/decryption operations
data FF3Cipher = FF3Cipher
  { cipherRadix    :: !Int        -- ^ Radix for the character set
  , cipherAES      :: !AESCipher  -- ^ AES cipher instance  
  , cipherTweak    :: !ByteString -- ^ Base tweak (8 bytes)
  , cipherMinLen   :: !Int        -- ^ Minimum plaintext length
  , cipherMaxLen   :: !Int        -- ^ Maximum plaintext length
  } deriving (Show)

-- | AES cipher wrapper to abstract over different key sizes
-- Using existential types to handle AES128/192/256 uniformly
data AESCipher = AES128Cipher !AES128
               | AES192Cipher !AES192
               | AES256Cipher !AES256

-- Manual Show instance since AES types don't have Show instances
instance Show AESCipher where
  show (AES128Cipher _) = "AES128Cipher <AES128>"
  show (AES192Cipher _) = "AES192Cipher <AES192>"
  show (AES256Cipher _) = "AES256Cipher <AES256>"

-- | FF3 error types for pure error handling
data FF3Error = InvalidRadix Int
              | InvalidKeyLength Int  
              | InvalidTweakLength Int
              | InvalidLength Int Int Int  -- ^ actual min max
              | AESError CryptoError
              deriving (Show, Eq)

-- | Create a new FF3 cipher - pure function with explicit error handling
-- Uses Maybe/Either for error handling instead of exceptions
ff3Cipher :: Int -> ByteString -> ByteString -> Either FF3Error FF3Cipher
ff3Cipher radix key tweak
  | radix < 2 || radix > 62 = Left $ InvalidRadix radix
  | BS.length tweak /= 8 = Left $ InvalidTweakLength (BS.length tweak)  
  | otherwise = do
      aesCipher <- createAESCipher key
      let maxLen = if radix >= 2 && radix <= 36 then 32 else 56
      pure $ FF3Cipher 
        { cipherRadix = radix
        , cipherAES = aesCipher
        , cipherTweak = tweak
        , cipherMinLen = 2
        , cipherMaxLen = maxLen
        }

-- | Create AES cipher with FF3 byte-reversal convention  
-- Pure function that handles different AES key sizes
createAESCipher :: ByteString -> Either FF3Error AESCipher
createAESCipher key = 
  case BS.length key of
    16 -> case cipherInit (reverseBytes key) of
            CryptoFailed err -> Left $ AESError err
            CryptoPassed aes -> Right $ AES128Cipher aes
    24 -> case cipherInit (reverseBytes key) of
            CryptoFailed err -> Left $ AESError err  
            CryptoPassed aes -> Right $ AES192Cipher aes
    32 -> case cipherInit (reverseBytes key) of
            CryptoFailed err -> Left $ AESError err
            CryptoPassed aes -> Right $ AES256Cipher aes
    n -> Left $ InvalidKeyLength n

-- | Encrypt digits using FF3 algorithm - pure function
encryptDigits :: FF3Cipher -> [Int] -> Maybe ByteString -> Either FF3Error [Int]
encryptDigits cipher plaintext additionalTweak = do
  let n = length plaintext
  when (n < cipherMinLen cipher || n > cipherMaxLen cipher) $
    Left $ InvalidLength n (cipherMinLen cipher) (cipherMaxLen cipher)
  let effectiveTweak = combineTweaks (cipherTweak cipher) additionalTweak
  pure $ ff3Encrypt cipher plaintext effectiveTweak

-- | Decrypt digits using FF3 algorithm - pure function  
decryptDigits :: FF3Cipher -> [Int] -> Maybe ByteString -> Either FF3Error [Int]
decryptDigits cipher ciphertext additionalTweak = do
  let n = length ciphertext
  when (n < cipherMinLen cipher || n > cipherMaxLen cipher) $
    Left $ InvalidLength n (cipherMinLen cipher) (cipherMaxLen cipher)
  let effectiveTweak = combineTweaks (cipherTweak cipher) additionalTweak
  pure $ ff3Decrypt cipher ciphertext effectiveTweak

-- | Core FF3 encryption algorithm - pure functional implementation
-- Uses immutable data structures and pure functions throughout
ff3Encrypt :: FF3Cipher -> [Int] -> ByteString -> [Int]
ff3Encrypt cipher plaintext tweak = 
  let n = length plaintext
      u = (n + 1) `div` 2  -- ceiling(n/2)
      v = n - u
      (initialA, initialB) = splitAt u plaintext
      
      -- Perform 8 Feistel rounds using fold for functional iteration
      (finalA, finalB) = foldl' (feistelRound cipher tweak u v) (initialA, initialB) [0..7]
  in finalA ++ finalB

-- | Core FF3 decryption algorithm - pure functional implementation
ff3Decrypt :: FF3Cipher -> [Int] -> ByteString -> [Int]  
ff3Decrypt cipher ciphertext tweak =
  let n = length ciphertext
      u = (n + 1) `div` 2
      v = n - u  
      (initialA, initialB) = splitAt u ciphertext
      
      -- Perform 8 Feistel rounds in reverse using fold
      (finalA, finalB) = foldl' (feistelRoundReverse cipher tweak u v) (initialA, initialB) [7,6..0]
  in finalA ++ finalB

-- | Single Feistel round for encryption - pure function
feistelRound :: FF3Cipher -> ByteString -> Int -> Int -> ([Int], [Int]) -> Int -> ([Int], [Int])
feistelRound cipher tweak u v (a, b) i
  | even i = 
      let w = calculateW tweak i
          p = calculateP cipher i w b
          m = radixPower (cipherRadix cipher) u
          aReversed = reverse a
          aNum = digitsToInteger (cipherRadix cipher) aReversed
          y = (aNum + p) `mod` m
          newDigits = integerToDigits (cipherRadix cipher) y u
          newA = reverse newDigits
      in (newA, b)
  | otherwise =
      let w = calculateW tweak i  
          p = calculateP cipher i w a
          m = radixPower (cipherRadix cipher) v
          bReversed = reverse b
          bNum = digitsToInteger (cipherRadix cipher) bReversed
          y = (bNum + p) `mod` m
          newDigits = integerToDigits (cipherRadix cipher) y v
          newB = reverse newDigits
      in (a, newB)

-- | Single Feistel round for decryption - pure function
feistelRoundReverse :: FF3Cipher -> ByteString -> Int -> Int -> ([Int], [Int]) -> Int -> ([Int], [Int])
feistelRoundReverse cipher tweak u v (a, b) i
  | even i =
      let w = calculateW tweak i
          p = calculateP cipher i w b  
          m = radixPower (cipherRadix cipher) u
          aReversed = reverse a
          aNum = digitsToInteger (cipherRadix cipher) aReversed
          -- Modular subtraction: (a - b) mod m = (a + m - (b mod m)) mod m
          pMod = p `mod` m
          y = (aNum + m - pMod) `mod` m
          newDigits = integerToDigits (cipherRadix cipher) y u
          newA = reverse newDigits
      in (newA, b)
  | otherwise =
      let w = calculateW tweak i
          p = calculateP cipher i w a
          m = radixPower (cipherRadix cipher) v  
          bReversed = reverse b
          bNum = digitsToInteger (cipherRadix cipher) bReversed
          -- Modular subtraction  
          pMod = p `mod` m
          y = (bNum + m - pMod) `mod` m
          newDigits = integerToDigits (cipherRadix cipher) y v
          newB = reverse newDigits
      in (a, newB)

-- | Calculate W parameter for FF3 round function - pure function
-- Follows NIST FF3 specification exactly
calculateW :: ByteString -> Int -> ByteString
calculateW tweak roundNum =
  if even roundNum
    then BS.drop 4 tweak  -- Even rounds: Tr (rightmost 4 bytes)
    else BS.take 4 tweak  -- Odd rounds: Tl (leftmost 4 bytes)

-- | Calculate P parameter using AES encryption - functional approach
calculateP :: FF3Cipher -> Int -> ByteString -> [Int] -> Integer
calculateP cipher roundNum w block =
  let input = createPInput w roundNum block cipher
      reversedInput = reverseBytes input
      aesOutput = encryptAES (cipherAES cipher) reversedInput  
      output = reverseBytes aesOutput
  in bytesToInteger output

-- | Create P calculation input - pure function
createPInput :: ByteString -> Int -> [Int] -> FF3Cipher -> ByteString
createPInput w roundNum block cipher =
  let -- First 4 bytes: W XOR round number in last byte
      wArray = BS.unpack w
      roundByte = fromIntegral roundNum
      firstFour = BS.pack $ take 3 wArray ++ [last wArray `xor` roundByte]
      
      -- Last 12 bytes: NUM_radix(REV(B))
      reversedBlock = reverse block
      blockNum = digitsToInteger (cipherRadix cipher) reversedBlock
      blockBytes = integerToBytes blockNum 12
      
  in firstFour <> blockBytes

-- | AES encryption dispatch - pure function  
encryptAES :: AESCipher -> ByteString -> ByteString
encryptAES (AES128Cipher aes) input = ecbEncrypt aes input
encryptAES (AES192Cipher aes) input = ecbEncrypt aes input  
encryptAES (AES256Cipher aes) input = ecbEncrypt aes input

-- | Helper Functions - All Pure and Functional

-- | Reverse bytes in ByteString - pure function
reverseBytes :: ByteString -> ByteString
reverseBytes = BS.reverse

-- | Combine base tweak with additional tweak - pure function
combineTweaks :: ByteString -> Maybe ByteString -> ByteString  
combineTweaks baseTweak Nothing = baseTweak
combineTweaks baseTweak (Just additional) =
  let baseArray = BS.unpack baseTweak
      additionalArray = BS.unpack additional
      combined = zipWith xor baseArray (additionalArray ++ repeat 0)
  in BS.pack $ take 8 combined

-- | Convert digit list to Integer using radix - pure fold
digitsToInteger :: Int -> [Int] -> Integer
digitsToInteger radix = foldl' (\acc d -> acc * fromIntegral radix + fromIntegral d) 0

-- | Convert Integer to digit list with padding - pure function
integerToDigits :: Int -> Integer -> Int -> [Int]
integerToDigits radix num len = 
  let digits = unfoldr' (\n -> if n == 0 then Nothing else 
                                let (q, r) = n `divMod` fromIntegral radix 
                                in Just (fromIntegral r, q)) num
      padded = digits ++ replicate (len - length digits) 0
  in reverse $ take len padded

-- | Unfold function (since it's not in Prelude) - pure function
unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f b = case f b of
  Nothing -> []
  Just (a, b') -> a : unfoldr' f b'

-- | Calculate radix^length efficiently - pure function
radixPower :: Int -> Int -> Integer  
radixPower radix len = fromIntegral radix ^ len

-- | Convert bytes to Integer - pure function
bytesToInteger :: ByteString -> Integer
bytesToInteger = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

-- | Convert Integer to ByteString with padding - pure function  
integerToBytes :: Integer -> Int -> ByteString
integerToBytes num len =
  let bytes = unfoldr' (\n -> if n == 0 then Nothing else 
                               let (q, r) = n `divMod` 256
                               in Just (fromIntegral r, q)) num
      padded = bytes ++ replicate (len - length bytes) 0
  in BS.pack $ reverse $ take len padded