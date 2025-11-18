{-|
Module      : FF3.API
Description : Pure functional public API with string ↔ digits conversion
Copyright   : (c) Horizon Digital Engineering, 2025
License     : BUSL-1.1
Maintainer  : engineering@horizondigital.dev
Stability   : experimental

FF3 API - High-level pure functional interface for format-preserving encryption.
This module provides monadic error handling and pure string transformations.
-}

{-# LANGUAGE OverloadedStrings #-}

module FF3.API
  ( -- * High-Level Types
    FF3(..)
  , FF3Error(..)
    -- * Cipher Construction
  , fromSpec
  , digits
  , hexLower
  , hexUpper
  , base36Lower
  , base36Upper
  , base62
  , hex
  , base36
  , radix26
    -- * Core Operations
  , encrypt
  , decrypt
  , encryptText
  , decryptText
  ) where

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Control.Monad (when)

import FF3.Core (FF3Cipher, FF3Error(..), ff3Cipher, encryptDigits, decryptDigits)
import FF3.Alphabets (AlphabetSpec(..), specDigits, specHexLower, specHexUpper,
                      specBase36Low, specBase36Up, specBase62, specRadix26)

-- | Lightweight cipher with alphabet-specific string conversion
-- Pure functional wrapper around core FF3 cipher
data FF3 = FF3
  { cipherCore :: !FF3Cipher      -- ^ Core FF3 cipher instance
  , cipherSpec :: !AlphabetSpec   -- ^ Alphabet specification
  } deriving (Show)

-- | Create cipher from alphabet specification - pure function with error handling
fromSpec :: ByteString -> ByteString -> AlphabetSpec -> Either FF3Error FF3
fromSpec key tweak spec = do
  when (length (charset spec) < 2) $
    Left $ InvalidRadix (length $ charset spec)
  core <- ff3Cipher (radix spec) key tweak
  pure $ FF3 
    { cipherCore = core
    , cipherSpec = spec  
    }

-- | Convenience constructors for standard alphabets - pure functions

digits :: ByteString -> ByteString -> Either FF3Error FF3
digits key tweak = fromSpec key tweak specDigits

hexLower :: ByteString -> ByteString -> Either FF3Error FF3
hexLower key tweak = fromSpec key tweak specHexLower

hexUpper :: ByteString -> ByteString -> Either FF3Error FF3
hexUpper key tweak = fromSpec key tweak specHexUpper

base36Lower :: ByteString -> ByteString -> Either FF3Error FF3
base36Lower key tweak = fromSpec key tweak specBase36Low

base36Upper :: ByteString -> ByteString -> Either FF3Error FF3
base36Upper key tweak = fromSpec key tweak specBase36Up

base62 :: ByteString -> ByteString -> Either FF3Error FF3
base62 key tweak = fromSpec key tweak specBase62

-- | Compatibility aliases - pure functions
hex :: ByteString -> ByteString -> Either FF3Error FF3
hex = hexLower

base36 :: ByteString -> ByteString -> Either FF3Error FF3
base36 = base36Lower

radix26 :: ByteString -> ByteString -> Either FF3Error FF3
radix26 key tweak = fromSpec key tweak specRadix26

-- | Encrypt string preserving exact format - pure function with monadic error handling
encryptText :: FF3 -> String -> Maybe ByteString -> Either String String
encryptText cipher plaintext additionalTweak = do
  plainDigits <- stringToDigits (cipherSpec cipher) plaintext
  encryptedDigits <- case encryptDigits (cipherCore cipher) plainDigits additionalTweak of
    Left err -> Left $ show err
    Right result -> Right result
  digitsToString (cipherSpec cipher) encryptedDigits

-- | Decrypt string preserving exact format - pure function with monadic error handling
decryptText :: FF3 -> String -> Maybe ByteString -> Either String String
decryptText cipher ciphertext additionalTweak = do
  cipherDigits <- stringToDigits (cipherSpec cipher) ciphertext
  decryptedDigits <- case decryptDigits (cipherCore cipher) cipherDigits additionalTweak of
    Left err -> Left $ show err
    Right result -> Right result
  digitsToString (cipherSpec cipher) decryptedDigits

-- | Shorter aliases for common use (no additional tweak)
encrypt :: FF3 -> String -> Either String String
encrypt cipher plaintext = encryptText cipher plaintext Nothing

decrypt :: FF3 -> String -> Either String String
decrypt cipher ciphertext = decryptText cipher ciphertext Nothing

-- | Pure Helper Functions for String ↔ Digits Conversion

-- | Convert string to digit array using alphabet - pure function with explicit error handling
stringToDigits :: AlphabetSpec -> String -> Either String [Int]
stringToDigits spec str =
  let convertChar :: (Int, Char) -> Either String Int
      convertChar (pos, char) =
        case Map.lookup char (charToInt spec) of
          Just digit -> Right digit
          Nothing -> Left $ "Invalid character '" ++ [char] ++ "' at position " ++ show pos
      results = mapM convertChar (zip [0..] str)
  in case results of
       Left err -> Left err
       Right digitList -> Right digitList

-- | Convert digit array to string using alphabet - pure function with error handling
digitsToString :: AlphabetSpec -> [Int] -> Either String String
digitsToString spec digitList =
  let convertDigit digit =
        case Map.lookup digit (intToChar spec) of
          Just char -> Right char
          Nothing -> Left $ "Digit " ++ show digit ++ " out of range for radix " ++ show (radix spec)
  in mapM convertDigit digitList