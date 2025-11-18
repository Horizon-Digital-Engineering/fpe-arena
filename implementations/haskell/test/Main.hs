{-|
Module      : Main
Description : FF3 test suite with NIST test vectors
Copyright   : (c) Horizon Digital Engineering, 2025
License     : BUSL-1.1
Maintainer  : engineering@horizondigital.dev

FF3 Test Suite - Comprehensive tests including NIST FF3-1 test vectors.
Validates pure functional implementation correctness.
-}

module Main (main) where

import Test.Hspec
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import FF3

-- | NIST FF3-1 test vectors for validation (Official NIST SP 800-38G)
-- Only using AES-128 vectors to keep tests simple and focused
nistTestVectors :: [(ByteString, ByteString, String, String)]
nistTestVectors =
  [ -- (key, tweak, plaintext, expected_ciphertext) - All official NIST vectors
    (BS.pack [0xEF, 0x43, 0x59, 0xD8, 0xD5, 0x80, 0xAA, 0x4F,
              0x7F, 0x03, 0x6D, 0x6F, 0x04, 0xFC, 0x6A, 0x94],
     BS.pack [0xD8, 0xE7, 0x92, 0x0A, 0xFA, 0x33, 0x0A, 0x73],
     "890121234567890000",
     "750918814058654607")

  , (BS.pack [0xEF, 0x43, 0x59, 0xD8, 0xD5, 0x80, 0xAA, 0x4F,
              0x7F, 0x03, 0x6D, 0x6F, 0x04, 0xFC, 0x6A, 0x94],
     BS.pack [0x9A, 0x76, 0x8A, 0x92, 0xF6, 0x0E, 0x12, 0xD8],
     "890121234567890000",
     "018989839189395384")

  , (BS.pack [0xEF, 0x43, 0x59, 0xD8, 0xD5, 0x80, 0xAA, 0x4F,
              0x7F, 0x03, 0x6D, 0x6F, 0x04, 0xFC, 0x6A, 0x94],
     BS.pack [0xD8, 0xE7, 0x92, 0x0A, 0xFA, 0x33, 0x0A, 0x73],
     "89012123456789000000789000000",
     "48598367162252569629397416226")

  , (BS.pack [0xEF, 0x43, 0x59, 0xD8, 0xD5, 0x80, 0xAA, 0x4F,
              0x7F, 0x03, 0x6D, 0x6F, 0x04, 0xFC, 0x6A, 0x94],
     BS.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
     "89012123456789000000789000000",
     "34695224821734535122613701434")
  ]

-- | Main test runner
main :: IO ()
main = hspec $ do
  describe "FF3 Core Functionality" $ do
    it "should create cipher successfully" $ do
      let key = BS.pack [0x2B, 0x7E, 0x15, 0x16, 0x28, 0xAE, 0xD2, 0xA6,
                         0xAB, 0xF7, 0x15, 0x88, 0x09, 0xCF, 0x4F, 0x3C]
      let tweak = BS.pack [0x39, 0x38, 0x37, 0x36, 0x35, 0x34, 0x33, 0x32]

      case digits key tweak of
        Left err -> expectationFailure $ "Failed to create cipher: " ++ show err
        Right _ -> return ()

    it "should perform round-trip encryption/decryption" $ do
      let key = BS.pack [0x2B, 0x7E, 0x15, 0x16, 0x28, 0xAE, 0xD2, 0xA6,
                         0xAB, 0xF7, 0x15, 0x88, 0x09, 0xCF, 0x4F, 0x3C]
      let tweak = BS.pack [0x39, 0x38, 0x37, 0x36, 0x35, 0x34, 0x33, 0x32]
      let plaintext = "1234567890"

      case digits key tweak of
        Left err -> expectationFailure $ "Failed to create cipher: " ++ show err
        Right cipher -> do
          case encryptText cipher plaintext Nothing of
            Left err -> expectationFailure $ "Encryption failed: " ++ err
            Right ciphertext -> do
              case decryptText cipher ciphertext Nothing of
                Left err -> expectationFailure $ "Decryption failed: " ++ err
                Right decrypted -> decrypted `shouldBe` plaintext

  describe "NIST Test Vectors" $ do
    mapM_ testVector (zip [1..] nistTestVectors)

-- | Test individual NIST vector
testVector :: (Int, (ByteString, ByteString, String, String)) -> Spec
testVector (i, (key, tweak, plaintext, expected)) =
  it ("should pass NIST test vector " ++ show i) $ do
    case digits key tweak of
      Left err -> expectationFailure $ "Failed to create cipher: " ++ show err
      Right cipher -> do
        case encryptText cipher plaintext Nothing of
          Left err -> expectationFailure $ "Encryption failed: " ++ err
          Right ciphertext -> ciphertext `shouldBe` expected