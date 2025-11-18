{-|
Module      : Main
Description : FF3 CLI Tool
Copyright   : (c) Horizon Digital Engineering, 2025
License     : BUSL-1.1
Maintainer  : engineering@horizondigital.dev

FF3 CLI - Command-line interface for FF3 encryption/decryption operations.

⚠️ FF3 was withdrawn by NIST due to security vulnerabilities.
This implementation is for educational and research purposes only.
-}

module Main (main) where

import qualified Data.ByteString as BS
import Data.Char (toLower)
import System.Environment (getArgs)
import Numeric (readHex)

import FF3

-- | Convert hex string to ByteString
hexToByteString :: String -> Maybe BS.ByteString
hexToByteString [] = Just BS.empty
hexToByteString [_] = Nothing  -- Odd number of hex digits
hexToByteString (a:b:rest) = do
  byte <- case readHex [a,b] of
    [(n, "")] -> Just (fromIntegral (n :: Integer))
    _ -> Nothing
  remaining <- hexToByteString rest
  return $ BS.cons byte remaining

-- | Show usage information
showUsage :: IO ()
showUsage = putStrLn $ unlines
  [ ""
  , "FF3 Format Preserving Encryption CLI v0.1.0"
  , ""
  , "⚠️  FF3 was WITHDRAWN by NIST due to security vulnerabilities."
  , "   This implementation is for EDUCATIONAL and RESEARCH purposes only."
  , ""
  , "Usage: ff3-cli [options]"
  , ""
  , "Options:"
  , "  -h, --help                 Show this help message"
  , "  -e, --encrypt TEXT         Encrypt the given text"
  , "  -d, --decrypt TEXT         Decrypt the given text"
  , "  -k, --key HEX             Key in hexadecimal format (required)"
  , "  -t, --tweak HEX           Tweak in hexadecimal format (required)"
  , "  -a, --alphabet TYPE       Alphabet type (default: digits)"
  , "  -v, --version             Show version"
  , ""
  , "Alphabet Types:"
  , "  digits        0123456789"
  , "  hex-lower     0123456789abcdef"
  , "  hex-upper     0123456789ABCDEF"
  , "  base36-lower  0123456789abcdefghijklmnopqrstuvwxyz"
  , "  base36-upper  0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  , "  base62        0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  , ""
  , "Examples:"
  , "  ff3-cli -e \"1234567890\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73"
  , "  ff3-cli -d \"750918814058654607\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73"
  , ""
  ]

-- | Show security warning
showSecurityWarning :: IO ()
showSecurityWarning = putStrLn $ unlines
  [ ""
  , "⚠️  FF3 Format Preserving Encryption CLI v0.1.0"
  , ""
  , "    FF3 was WITHDRAWN by NIST due to security vulnerabilities."
  , "    This implementation is for EDUCATIONAL and RESEARCH purposes only."
  , ""
  , "    DO NOT use in production systems."
  , ""
  , "    Research reference: NIST SP 800-38G (withdrawn)"
  , ""
  ]

-- | Create cipher based on alphabet type
createCipher :: BS.ByteString -> BS.ByteString -> String -> Either String FF3
createCipher key tweak alphabetType =
  case map toLower alphabetType of
    "digits"       -> case digits key tweak of
                        Left err -> Left (show err)
                        Right cipher -> Right cipher
    "hex-lower"    -> case hexLower key tweak of
                        Left err -> Left (show err)
                        Right cipher -> Right cipher
    "hex-upper"    -> case hexUpper key tweak of
                        Left err -> Left (show err)
                        Right cipher -> Right cipher
    "base36-lower" -> case base36Lower key tweak of
                        Left err -> Left (show err)
                        Right cipher -> Right cipher
    "base36-upper" -> case base36Upper key tweak of
                        Left err -> Left (show err)
                        Right cipher -> Right cipher
    "base62"       -> case base62 key tweak of
                        Left err -> Left (show err)
                        Right cipher -> Right cipher
    _              -> Left $ "Unknown alphabet type: " ++ alphabetType

-- | Interactive mode
-- | Command line mode
commandLineMode :: [String] -> IO ()
commandLineMode args = do
  let parsed = parseArgs args
  case parsed of
    Left err -> putStrLn $ "❌ Error: " ++ err
    Right opts -> executeCommand opts

-- | Command line options
data Options = Options
  { optEncrypt :: Maybe String
  , optDecrypt :: Maybe String
  , optKey :: Maybe String
  , optTweak :: Maybe String
  , optAlphabet :: String
  } deriving (Show)

-- | Parse command line arguments
parseArgs :: [String] -> Either String Options
parseArgs args = parseArgs' args (Options Nothing Nothing Nothing Nothing "digits")
  where
    parseArgs' [] opts = Right opts
    parseArgs' ("-h":_) _ = Left "help"
    parseArgs' ("--help":_) _ = Left "help"
    parseArgs' ("-v":_) _ = Left "version"
    parseArgs' ("--version":_) _ = Left "version"
    parseArgs' ("-e":text:rest) opts = parseArgs' rest opts { optEncrypt = Just text }
    parseArgs' ("--encrypt":text:rest) opts = parseArgs' rest opts { optEncrypt = Just text }
    parseArgs' ("-d":text:rest) opts = parseArgs' rest opts { optDecrypt = Just text }
    parseArgs' ("--decrypt":text:rest) opts = parseArgs' rest opts { optDecrypt = Just text }
    parseArgs' ("-k":key:rest) opts = parseArgs' rest opts { optKey = Just key }
    parseArgs' ("--key":key:rest) opts = parseArgs' rest opts { optKey = Just key }
    parseArgs' ("-t":tweak:rest) opts = parseArgs' rest opts { optTweak = Just tweak }
    parseArgs' ("--tweak":tweak:rest) opts = parseArgs' rest opts { optTweak = Just tweak }
    parseArgs' ("-a":alphabet:rest) opts = parseArgs' rest opts { optAlphabet = alphabet }
    parseArgs' ("--alphabet":alphabet:rest) opts = parseArgs' rest opts { optAlphabet = alphabet }
    parseArgs' (unknown:_) _ = Left $ "Unknown option: " ++ unknown

-- | Execute command based on options
executeCommand :: Options -> IO ()
executeCommand opts = do
  case (optKey opts, optTweak opts) of
    (Nothing, _) -> putStrLn "❌ Error: Key (-k) is required"
    (_, Nothing) -> putStrLn "❌ Error: Tweak (-t) is required"
    (Just keyHex, Just tweakHex) -> do
      case (hexToByteString keyHex, hexToByteString tweakHex) of
        (Nothing, _) -> putStrLn "❌ Error: Invalid key format"
        (_, Nothing) -> putStrLn "❌ Error: Invalid tweak format"
        (Just key, Just tweak) -> do
          case createCipher key tweak (optAlphabet opts) of
            Left err -> putStrLn $ "❌ Error: " ++ err
            Right cipher -> do
              case (optEncrypt opts, optDecrypt opts) of
                (Nothing, Nothing) -> putStrLn "❌ Error: Either --encrypt or --decrypt must be specified"
                (Just plaintext, _) -> do
                  case encryptText cipher plaintext Nothing of
                    Left err -> putStrLn $ "❌ Error: " ++ err
                    Right ciphertext -> putStrLn ciphertext
                (_, Just ciphertext) -> do
                  case decryptText cipher ciphertext Nothing of
                    Left err -> putStrLn $ "❌ Error: " ++ err
                    Right plaintext -> putStrLn plaintext

-- | Main function
main :: IO ()
main = do
  showSecurityWarning
  args <- getArgs
  case args of
    [] -> showUsage
    ["-h"] -> showUsage
    ["--help"] -> showUsage
    ["-v"] -> putStrLn "FF3 CLI v0.1.0"
    ["--version"] -> putStrLn "FF3 CLI v0.1.0"
    _ -> commandLineMode args