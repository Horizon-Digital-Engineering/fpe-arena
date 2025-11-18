{-|
Module      : Main
Description : FF3 NIST Test Vector Validation Tool
-}

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Aeson
import Data.Text (Text)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.Directory (doesFileExist)
import System.IO (stderr, hPutStrLn)
import Control.Monad (unless, when)
import FF3

data ValidateOptions = ValidateOptions
  { optVectorsPath :: Maybe FilePath
  , optVerbose :: Bool
  , optQuiet :: Bool
  , optHelp :: Bool
  } deriving (Show)

defaultValidateOptions :: ValidateOptions
defaultValidateOptions = ValidateOptions
  { optVectorsPath = Nothing
  , optVerbose = False
  , optQuiet = False
  , optHelp = False
  }

data NistTestVector = NistTestVector
  { sample :: Int
  , algorithm :: Text
  , key :: Text
  , vectorRadix :: Int
  , plaintext :: Text
  , tweak :: Text
  , ciphertext :: Text
  } deriving (Show)

instance FromJSON NistTestVector where
  parseJSON = withObject "NistTestVector" $ \o -> NistTestVector
    <$> o .: "sample"
    <*> o .: "algorithm"
    <*> o .: "key"
    <*> o .: "radix"
    <*> o .: "plaintext"
    <*> o .: "tweak"
    <*> o .: "ciphertext"

data NistTestVectors = NistTestVectors
  { description :: Text
  , source :: Text
  , vectors :: [NistTestVector]
  } deriving (Show)

instance FromJSON NistTestVectors where
  parseJSON = withObject "NistTestVectors" $ \o -> NistTestVectors
    <$> o .: "description"
    <*> o .: "source"
    <*> o .: "vectors"

showUsage :: IO ()
showUsage = do
  putStrLn "FF3 NIST Test Vector Validation Tool\n"
  putStrLn "Usage: ff3-validate [OPTIONS]\n"
  putStrLn "Options:"
  putStrLn "  --vectors PATH    Path to test vectors JSON file"
  putStrLn "  --verbose         Show detailed test output"
  putStrLn "  --quiet           Only show failures and summary"
  putStrLn "  -h, --help        Show this help message\n"

parseArgs :: [String] -> Either String ValidateOptions
parseArgs = go defaultValidateOptions
  where
    go opts [] = Right opts
    go opts ("-h":_) = Right opts { optHelp = True }
    go opts ("--help":_) = Right opts { optHelp = True }
    go opts ("--vectors":path:rest) = go (opts { optVectorsPath = Just path }) rest
    go _ ("--vectors":_) = Left "Missing value for --vectors"
    go opts ("--verbose":rest) = go (opts { optVerbose = True }) rest
    go opts ("--quiet":rest) = go (opts { optQuiet = True }) rest
    go _ (arg:_) = Left $ "Unknown option: " ++ arg

hexToByteString :: String -> Maybe BS.ByteString
hexToByteString [] = Just BS.empty
hexToByteString [_] = Nothing
hexToByteString (a:b:rest) = do
  byte <- case reads ("0x" ++ [a,b]) of
    [(n, "")] -> Just (fromIntegral (n :: Integer))
    _ -> Nothing
  remaining <- hexToByteString rest
  return $ BS.cons byte remaining

createCipherForRadix :: BS.ByteString -> BS.ByteString -> Int -> Either String FF3
createCipherForRadix keyBS tweakBS r =
  case r of
    10 -> case digits keyBS tweakBS of
            Left err -> Left (show err)
            Right cipher -> Right cipher
    16 -> case hexLower keyBS tweakBS of
            Left err -> Left (show err)
            Right cipher -> Right cipher
    26 -> case radix26 keyBS tweakBS of
            Left err -> Left (show err)
            Right cipher -> Right cipher
    36 -> case base36Lower keyBS tweakBS of
            Left err -> Left (show err)
            Right cipher -> Right cipher
    62 -> case base62 keyBS tweakBS of
            Left err -> Left (show err)
            Right cipher -> Right cipher
    _  -> Left $ "Unsupported radix " ++ show r

findVectorsFile :: Maybe FilePath -> IO (Maybe FilePath)
findVectorsFile (Just path) = do
  exists <- doesFileExist path
  return $ if exists then Just path else Nothing
findVectorsFile Nothing = do
  envPath <- lookupEnv "FF3_TEST_VECTORS_PATH"
  let paths = case envPath of
        Just p -> [p, "../../shared/test-vectors/nist_ff3_official_vectors.json",
                   "../../../shared/test-vectors/nist_ff3_official_vectors.json",
                   "./nist_ff3_official_vectors.json"]
        Nothing -> ["../../shared/test-vectors/nist_ff3_official_vectors.json",
                    "../../../shared/test-vectors/nist_ff3_official_vectors.json",
                    "./nist_ff3_official_vectors.json"]
  findFirst paths
  where
    findFirst [] = return Nothing
    findFirst (p:ps) = do
      exists <- doesFileExist p
      if exists then return (Just p) else findFirst ps

runValidation :: ValidateOptions -> FilePath -> IO ()
runValidation opts vectorsPath = do
  unless (optQuiet opts) $ do
    putStrLn "FF3 NIST Test Vector Validation Tool"
    putStrLn "========================================\n"
    putStrLn $ "Vector file: " ++ vectorsPath ++ "\n"

  jsonData <- BS.readFile vectorsPath
  case eitherDecodeStrict jsonData of
    Left err -> do
      hPutStrLn stderr $ "Error parsing JSON: " ++ err
      exitWith (ExitFailure 2)
    Right testVectors -> do
      unless (optQuiet opts) $
        putStrLn $ "Testing " ++ show (length $ vectors testVectors) ++ " NIST FF3 vectors...\n"

      (passed, failed) <- validateVectors opts (vectors testVectors)

      unless (optQuiet opts) $ do
        putStrLn "\n========================================"
        putStrLn $ "Results: " ++ show passed ++ "/" ++ show (length $ vectors testVectors) ++ " passed\n"

      if failed == 0
        then do
          unless (optQuiet opts) $ do
            putStrLn "ALL NIST TEST VECTORS PASSED!\n"
            putStrLn "WARNING: FF3 was WITHDRAWN by NIST due to security vulnerabilities."
            putStrLn "This implementation is for EDUCATIONAL and RESEARCH purposes only."
            putStrLn "DO NOT use in production systems.\n"
          exitSuccess
        else do
          unless (optQuiet opts) $
            putStrLn "VALIDATION FAILED\n"
          exitWith (ExitFailure 1)

validateVectors :: ValidateOptions -> [NistTestVector] -> IO (Int, Int)
validateVectors opts vs = go 0 0 vs
  where
    go passed failed [] = return (passed, failed)
    go passed failed (v:rest) = do
      result <- validateVector opts v
      if result
        then go (passed + 1) failed rest
        else go passed (failed + 1) rest

validateVector :: ValidateOptions -> NistTestVector -> IO Bool
validateVector opts v = do
  let keyBS = hexToByteString (T.unpack $ key v)
      tweakBS = hexToByteString (T.unpack $ tweak v)

  case (keyBS, tweakBS) of
    (Nothing, _) -> do
      unless (optQuiet opts) $
        putStrLn $ "Sample " ++ show (sample v) ++ ": ERROR - Invalid key hex"
      return False
    (_, Nothing) -> do
      unless (optQuiet opts) $
        putStrLn $ "Sample " ++ show (sample v) ++ ": ERROR - Invalid tweak hex"
      return False
    (Just k, Just t) -> do
      case createCipherForRadix k t (vectorRadix v) of
        Left err -> do
          unless (optQuiet opts) $
            putStrLn $ "Sample " ++ show (sample v) ++ ": ERROR - " ++ err
          return False
        Right cipher -> do
          case encrypt cipher (T.unpack $ plaintext v) of
            Left err -> do
              unless (optQuiet opts) $
                putStrLn $ "Sample " ++ show (sample v) ++ ": ERROR - " ++ show err
              return False
            Right encrypted -> do
              let encryptPassed = encrypted == T.unpack (ciphertext v)
              roundtripPassed <- if encryptPassed
                then case decrypt cipher encrypted of
                  Left _ -> return False
                  Right decrypted -> return $ decrypted == T.unpack (plaintext v)
                else return True

              let testPassed = encryptPassed && roundtripPassed

              if testPassed
                then do
                  when (optVerbose opts) $
                    putStrLn $ "Sample " ++ show (sample v) ++ ": PASS"
                  return True
                else do
                  unless (optQuiet opts) $ do
                    putStrLn $ "Sample " ++ show (sample v) ++ ": FAIL"
                    unless encryptPassed $
                      putStrLn $ "  Expected: " ++ T.unpack (ciphertext v) ++
                                 "\n  Got:      " ++ encrypted
                    unless roundtripPassed $
                      putStrLn "  Round-trip failed"
                  return False

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr $ "Error: " ++ err
      showUsage
      exitWith (ExitFailure 1)
    Right opts -> do
      if optHelp opts
        then showUsage >> exitSuccess
        else do
          maybePath <- findVectorsFile (optVectorsPath opts)
          case maybePath of
            Nothing -> do
              case optVectorsPath opts of
                Just path -> hPutStrLn stderr $ "Error: Vectors file not found: " ++ path
                Nothing -> do
                  hPutStrLn stderr "Error: Could not find NIST test vectors file"
                  hPutStrLn stderr "Try: ff3-validate --vectors /path/to/vectors.json"
              exitWith (ExitFailure 2)
            Just path -> runValidation opts path
