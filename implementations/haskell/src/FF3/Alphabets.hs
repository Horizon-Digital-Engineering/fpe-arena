{-|
Module      : FF3.Alphabets
Description : Canonical alphabets for cross-language FF3 compatibility  
Copyright   : (c) Horizon Digital Engineering, 2025
License     : BUSL-1.1
Maintainer  : engineering@horizondigital.dev
Stability   : experimental

FF3 Alphabets - Pure functional alphabet definitions for format preservation.
This module provides canonical character sets in immutable, referentially transparent style.
-}

module FF3.Alphabets
  ( -- * Alphabet Specifications
    AlphabetSpec(..)
  , mkAlphabetSpec
    -- * Canonical Alphabets
  , alphaDigits
  , alphaHexLower
  , alphaHexUpper  
  , alphaBase36Low
  , alphaBase36Up
  , alphaBase62
    -- * Built-in Specifications
  , specDigits
  , specHexLower
  , specHexUpper
  , specBase36Low
  , specBase36Up
  , specBase62
  , specRadix26
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

-- | Alphabet specification for format-preserving encryption
-- Pure immutable data structure - NO NORMALIZATION, exact format preservation
data AlphabetSpec = AlphabetSpec
  { charset    :: String        -- ^ The character set defining valid symbols
  , charToInt  :: Map Char Int  -- ^ Pure function mapping chars to integers  
  , intToChar  :: Map Int Char  -- ^ Pure function mapping integers to chars
  , radix      :: Int           -- ^ Size of the alphabet (length charset)
  } deriving (Show, Eq)

-- | Smart constructor for AlphabetSpec with automatic index generation
-- Uses pure functional approach - no side effects, referentially transparent
mkAlphabetSpec :: String -> Maybe AlphabetSpec
mkAlphabetSpec cs
  | length cs < 2 = Nothing
  | length cs /= length (Map.keys charMap) = Nothing -- No duplicate chars
  | otherwise = Just $ AlphabetSpec
      { charset = cs
      , charToInt = charMap  
      , intToChar = intMap
      , radix = length cs
      }
  where
    charMap = Map.fromList $ zip cs [0..]
    intMap = Map.fromList $ zip [0..] cs

-- | Canonical alphabets (order is part of the specification!)
-- These are pure values - no computation, just constants

alphaDigits :: String
alphaDigits = "0123456789"

alphaHexLower :: String 
alphaHexLower = "0123456789abcdef"

alphaHexUpper :: String
alphaHexUpper = "0123456789ABCDEF"

alphaBase36Low :: String
alphaBase36Low = "0123456789abcdefghijklmnopqrstuvwxyz"

alphaBase36Up :: String  
alphaBase36Up = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphaBase62 :: String
alphaBase62 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

-- | Built-in alphabet specifications
-- Using 'fromJust' because we know these alphabets are valid by construction
-- In pure functional style, we prefer total functions and known-safe partial applications

specDigits :: AlphabetSpec
specDigits = fromJust $ mkAlphabetSpec alphaDigits

specHexLower :: AlphabetSpec
specHexLower = fromJust $ mkAlphabetSpec alphaHexLower

specHexUpper :: AlphabetSpec
specHexUpper = fromJust $ mkAlphabetSpec alphaHexUpper

specBase36Low :: AlphabetSpec
specBase36Low = fromJust $ mkAlphabetSpec alphaBase36Low

specBase36Up :: AlphabetSpec  
specBase36Up = fromJust $ mkAlphabetSpec alphaBase36Up

specBase62 :: AlphabetSpec
specBase62 = fromJust $ mkAlphabetSpec alphaBase62

-- | Special case: radix 26 using first 26 characters of base36 (0-9a-p)
specRadix26 :: AlphabetSpec
specRadix26 = fromJust $ mkAlphabetSpec (take 26 alphaBase36Low)