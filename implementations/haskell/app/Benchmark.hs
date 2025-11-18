{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-|
Module      : Main
Description : Spec-Compliant FF3 Performance Benchmark
Copyright   : (c) Horizon Digital Engineering, 2025
License     : BUSL-1.1
-}

module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Aeson (ToJSON, encode, eitherDecode, (.=), object)
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import Control.Monad (forM, forM_, when)
import Control.DeepSeq (deepseq, force)
import System.Environment (getArgs)
import System.Random (mkStdGen, randomRs)
import Data.Bits (xor)
import Data.Char (ord)
import GHC.Conc (getNumProcessors)
import System.Info (os, arch)

import FF3

-- Data structures matching the spec
data BenchmarkConfig = BenchmarkConfig
  { cfgAlphabet :: !String
  , cfgRadix :: !Int
  , cfgIterations :: !Int
  , cfgWarmup :: !Int
  , cfgLengths :: ![Int]
  , cfgCases :: ![String]
  , cfgKey :: !String
  , cfgTweak :: !String
  , cfgSeed :: !Int
  , cfgVerbose :: !Bool
  , cfgJsonOut :: !(Maybe FilePath)
  } deriving (Show, Generic)

instance A.FromJSON BenchmarkConfig where
  parseJSON = A.withObject "BenchmarkConfig" $ \v -> BenchmarkConfig
    <$> v A..:? "alphabet" A..!= "digits"
    <*> v A..:? "radix" A..!= 10
    <*> v A..:? "iterations" A..!= 100000
    <*> v A..:? "warmup" A..!= 10000
    <*> v A..:? "lengths" A..!= [9, 12, 16]
    <*> v A..:? "cases" A..!= ["enc", "dec", "roundtrip"]
    <*> v A..:? "key" A..!= "EF4359D8D580AA4F7F036D6F04FC6A94"
    <*> v A..:? "tweak" A..!= "D8E7920AFA330A73"
    <*> v A..:? "seed" A..!= 42
    <*> pure False  -- verbose not in JSON
    <*> pure Nothing  -- json_out not in JSON

defaultConfig :: BenchmarkConfig
defaultConfig = BenchmarkConfig
  { cfgAlphabet = "digits"
  , cfgRadix = 10
  , cfgIterations = 100000
  , cfgWarmup = 10000
  , cfgLengths = [9, 12, 16]
  , cfgCases = ["enc", "dec", "roundtrip"]
  , cfgKey = "EF4359D8D580AA4F7F036D6F04FC6A94"
  , cfgTweak = "D8E7920AFA330A73"
  , cfgSeed = 42
  , cfgVerbose = False
  , cfgJsonOut = Nothing
  }

data BenchmarkParameters = BenchmarkParameters
  { paramAlphabet :: !String
  , paramRadix :: !Int
  , paramLength :: !Int
  , paramKeyBits :: !Int
  , paramKeyFingerprint :: !String
  , paramTweak :: !String
  } deriving (Show, Generic)

instance ToJSON BenchmarkParameters where
  toJSON p = object
    [ "alphabet" .= paramAlphabet p
    , "radix" .= paramRadix p
    , "length" .= paramLength p
    , "key_bits" .= paramKeyBits p
    , "key_fingerprint" .= paramKeyFingerprint p
    , "tweak" .= paramTweak p
    ]

data BenchmarkResult = BenchmarkResult
  { resName :: !String
  , resTestCase :: !String
  , resParameters :: !BenchmarkParameters
  , resIterations :: !Int
  , resElapsedNs :: !Integer
  , resNsPerOp :: !Double
  , resOpsPerSec :: !Double
  , resChecksum :: !String
  } deriving (Show, Generic)

instance ToJSON BenchmarkResult where
  toJSON r = object
    [ "name" .= resName r
    , "test_case" .= resTestCase r
    , "parameters" .= resParameters r
    , "iterations" .= resIterations r
    , "elapsed_ns" .= resElapsedNs r
    , "ns_per_op" .= resNsPerOp r
    , "ops_per_sec" .= resOpsPerSec r
    , "checksum" .= resChecksum r
    ]

data PlatformInfo = PlatformInfo
  { platOS :: !String
  , platArch :: !String
  , platCPU :: !String
  , platCores :: !Int
  } deriving (Show, Generic)

instance ToJSON PlatformInfo where
  toJSON p = object
    [ "os" .= platOS p
    , "arch" .= platArch p
    , "cpu" .= platCPU p
    , "cores" .= platCores p
    ]

data BenchmarkReport = BenchmarkReport
  { repMetadata :: !A.Value
  , repConfiguration :: !A.Value
  , repBenchmarks :: ![BenchmarkResult]
  , repSummary :: !A.Value
  } deriving (Show, Generic)

instance ToJSON BenchmarkReport where
  toJSON r = object
    [ "metadata" .= repMetadata r
    , "configuration" .= repConfiguration r
    , "benchmarks" .= repBenchmarks r
    , "summary" .= repSummary r
    ]

-- Hash function for checksum
hashString :: String -> Int
hashString = foldl (\h c -> (h * 31 + ord c) `mod` 4294967296) 0

-- Generate ring buffer of inputs
generateInputs :: Int -> Int -> String -> Int -> [String]
generateInputs count len alphabet seed =
  take count $ chunks len $ randomRs (minChar, maxChar) (mkStdGen seed)
  where
    chars = case alphabet of
      "digits" -> "0123456789"
      "hex" -> "0123456789abcdef"
      "base36" -> "0123456789abcdefghijklmnopqrstuvwxyz"
      "base62" -> "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
      _ -> "0123456789"
    minChar = head chars
    maxChar = last chars
    chunks _ [] = []
    chunks n xs = let (chunk, rest) = splitAt n xs in chunk : chunks n rest

-- Run single benchmark with STRICT evaluation
runBenchmark :: FF3 -> String -> String -> Int -> Int -> Int -> Int -> ByteString -> String -> Int -> IO BenchmarkResult
runBenchmark cipher benchCase alphabet rdx len iterations warmup key tweakHex seed = do
  let ringSize = 64
  let inputs = generateInputs ringSize len alphabet seed

  -- Precompute ciphertexts for decrypt (FORCE evaluation!)
  let precomputedCTs = if benchCase == "dec"
        then map (\pt -> case encryptText cipher pt Nothing of
                           Right ct -> force ct
                           Left _ -> "") inputs
        else []

  -- Force evaluation of precomputed CTs
  precomputedCTs `deepseq` return ()

  -- Warmup
  forM_ [0..warmup-1] $ \i -> do
    let input = inputs !! (i `mod` ringSize)
    case benchCase of
      "enc" -> do
        let !result = case encryptText cipher input Nothing of
              Right ct -> ct
              Left _ -> ""
        result `deepseq` return ()
      "dec" -> do
        let !ct = precomputedCTs !! (i `mod` ringSize)
        let !result = case decryptText cipher ct Nothing of
              Right pt -> pt
              Left _ -> ""
        result `deepseq` return ()
      "roundtrip" -> do
        let !ct = case encryptText cipher input Nothing of
              Right c -> c
              Left _ -> ""
        let !result = case decryptText cipher ct Nothing of
              Right pt -> pt
              Left _ -> ""
        result `deepseq` return ()
      _ -> return ()

  -- Measured phase with STRICT evaluation
  start <- getCurrentTime
  let go !checksum !i
        | i >= iterations = return checksum
        | otherwise = do
            let input = inputs !! (i `mod` ringSize)
            let (!output, !newChecksum) = case benchCase of
                  "enc" -> case encryptText cipher input Nothing of
                    Right ct -> (ct, checksum `xor` hashString ct)
                    Left _ -> ("", checksum)
                  "dec" ->
                    let ct = precomputedCTs !! (i `mod` ringSize)
                    in case decryptText cipher ct Nothing of
                      Right pt -> (pt, checksum `xor` hashString pt)
                      Left _ -> ("", checksum)
                  "roundtrip" -> case encryptText cipher input Nothing of
                    Right ct -> case decryptText cipher ct Nothing of
                      Right pt -> (pt, checksum `xor` hashString pt)
                      Left _ -> ("", checksum)
                    Left _ -> ("", checksum)
                  _ -> ("", checksum)
            output `deepseq` go newChecksum (i + 1)

  !finalChecksum <- go 0 0
  end <- getCurrentTime

  let elapsed = diffUTCTime end start
  let elapsedNs = round (realToFrac elapsed * (1e9 :: Double)) :: Integer
  let nsPerOp = fromIntegral elapsedNs / fromInteger (toInteger iterations) :: Double
  let opsPerSec = (1e9 :: Double) / nsPerOp

  let keyFingerprint = take 8 $ concatMap (padHex . (`mod` 256) . fromIntegral) $ BS.unpack $ BS.take 4 key

  return $ BenchmarkResult
    { resName = benchCase ++ "_len" ++ show len ++ "_radix" ++ show rdx
    , resTestCase = benchCase
    , resParameters = BenchmarkParameters
        { paramAlphabet = alphabet
        , paramRadix = rdx
        , paramLength = len
        , paramKeyBits = BS.length key * 8
        , paramKeyFingerprint = map toUpper keyFingerprint
        , paramTweak = map toUpper tweakHex
        }
    , resIterations = iterations
    , resElapsedNs = elapsedNs
    , resNsPerOp = nsPerOp
    , resOpsPerSec = opsPerSec
    , resChecksum = padHex (finalChecksum `mod` 4294967296)
    }
  where
    padHex n = let h = showHex n "" in replicate (8 - length h) '0' ++ h
    toUpper c = if c >= 'a' && c <= 'f' then toEnum (fromEnum c - 32) else c

showHex :: Int -> ShowS
showHex n
  | n < 16 = showChar (hexDigit n)
  | otherwise = showHex (n `div` 16) . showChar (hexDigit (n `mod` 16))
  where
    hexDigit d
      | d < 10 = toEnum (fromEnum '0' + d)
      | otherwise = toEnum (fromEnum 'a' + d - 10)

hexStringToBytes :: String -> ByteString
hexStringToBytes [] = BS.empty
hexStringToBytes [_] = BS.empty  -- Odd length, ignore last char
hexStringToBytes (a:b:rest) = BS.cons (fromIntegral $ hexToInt a b) (hexStringToBytes rest)
  where
    hexToInt c1 c2 = hexDigitToInt c1 * 16 + hexDigitToInt c2
    hexDigitToInt c
      | c >= '0' && c <= '9' = ord c - ord '0'
      | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
      | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
      | otherwise = 0

main :: IO ()
main = do
  args <- getArgs

  when (elem "--help" args || elem "-h" args) $ do
    putStrLn "FF3 Performance Benchmark Tool\n"
    putStrLn "Usage: ff3-benchmark [OPTIONS]\n"
    putStrLn "Options:"
    putStrLn "  --config <file>    JSON config file"
    putStrLn "  --alphabet <name>  Alphabet: digits|hex|base36|base62 (default: digits)"
    putStrLn "  --radix <n>        Radix 2-62 (default: 10)"
    putStrLn "  --lengths <csv>    Comma-separated lengths (default: 9,12,16)"
    putStrLn "  --cases <csv>      Test cases: enc,dec,roundtrip (default: all)"
    putStrLn "  --iterations <n>   Measured iterations (default: 100000)"
    putStrLn "  --warmup <n>       Warmup iterations (default: 10000)"
    putStrLn "  --key <hex>        Hex-encoded key"
    putStrLn "  --tweak <hex>      Hex-encoded tweak"
    putStrLn "  --seed <n>         Random seed (default: 42)"
    putStrLn "  --quick            Reduce iterations 10x"
    putStrLn "  --json-out <file>  Write JSON results to file"
    putStrLn "  --verbose          Show progress messages"
    putStrLn "  -h, --help         Show this help"
    return ()

  config <- parseArgs defaultConfig args

  let key = hexStringToBytes (cfgKey config)
  let tweak = hexStringToBytes (cfgTweak config)
  let tweakHex = cfgTweak config

  let cipherResult = case cfgAlphabet config of
        "digits" -> digits key tweak
        "hex" -> hexLower key tweak
        "base36" -> base36Lower key tweak
        "base62" -> base62 key tweak
        _ -> digits key tweak

  case cipherResult of
    Left err -> putStrLn $ "Error: " ++ show err
    Right cipher -> do
      totalStart <- getCurrentTime

      results <- forM (cfgLengths config) $ \len -> do
        forM (cfgCases config) $ \benchCase -> do
          when (cfgVerbose config) $
            putStrLn $ "Running " ++ benchCase ++ "_len" ++ show len ++ "_radix" ++ show (cfgRadix config) ++ "..."
          runBenchmark cipher benchCase (cfgAlphabet config) (cfgRadix config) len (cfgIterations config) (cfgWarmup config) key tweakHex (cfgSeed config)

      totalEnd <- getCurrentTime
      let totalDuration = realToFrac $ diffUTCTime totalEnd totalStart

      cores <- getNumProcessors
      now <- getCurrentTime
      let timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now

      let allResults = concat results
      let overallChecksum = foldl (\acc r -> acc `xor` (read ("0x" ++ resChecksum r) :: Int)) 0 allResults

      let report = BenchmarkReport
            { repMetadata = object
                [ "version" .= ("1.0" :: String)
                , "timestamp" .= timestamp
                , "language" .= ("haskell" :: String)
                , "runtime" .= ("ghc" :: String)
                , "platform" .= object
                    [ "os" .= os
                    , "arch" .= arch
                    , "cpu" .= ("unknown" :: String)
                    , "cores" .= cores
                    ]
                ]
            , repConfiguration = object
                [ "seed" .= cfgSeed config
                , "warmup_iterations" .= cfgWarmup config
                ]
            , repBenchmarks = allResults
            , repSummary = object
                [ "total_tests" .= length allResults
                , "total_duration_sec" .= (totalDuration :: Double)
                , "checksum" .= padHex8 (overallChecksum `mod` 4294967296)
                ]
            }

      case cfgJsonOut config of
        Just path -> BS.writeFile path $ BS.toStrict $ encode report
        Nothing -> BS.putStr $ BS.toStrict $ encode report

  where
    padHex8 n = let h = showHex n "" in replicate (8 - length h) '0' ++ h

parseCSV :: String -> [Int]
parseCSV s = map read $ words $ map (\c -> if c == ',' then ' ' else c) s

parseCSVStr :: String -> [String]
parseCSVStr s = words $ map (\c -> if c == ',' then ' ' else c) s

parseArgs :: BenchmarkConfig -> [String] -> IO BenchmarkConfig
parseArgs cfg [] = return cfg
parseArgs cfg ("--config":path:rest) = do
  content <- BL.readFile path
  case eitherDecode content of
    Left err -> error $ "Error parsing config file: " ++ err
    Right newCfg -> parseArgs (newCfg { cfgVerbose = cfgVerbose cfg, cfgJsonOut = cfgJsonOut cfg }) rest
parseArgs cfg ("--alphabet":a:rest) = parseArgs (cfg { cfgAlphabet = a }) rest
parseArgs cfg ("--radix":n:rest) = parseArgs (cfg { cfgRadix = read n }) rest
parseArgs cfg ("--lengths":csv:rest) = parseArgs (cfg { cfgLengths = parseCSV csv }) rest
parseArgs cfg ("--cases":csv:rest) = parseArgs (cfg { cfgCases = parseCSVStr csv }) rest
parseArgs cfg ("--iterations":n:rest) = parseArgs (cfg { cfgIterations = read n }) rest
parseArgs cfg ("--warmup":n:rest) = parseArgs (cfg { cfgWarmup = read n }) rest
parseArgs cfg ("--key":k:rest) = parseArgs (cfg { cfgKey = k }) rest
parseArgs cfg ("--tweak":t:rest) = parseArgs (cfg { cfgTweak = t }) rest
parseArgs cfg ("--seed":n:rest) = parseArgs (cfg { cfgSeed = read n }) rest
parseArgs cfg ("--quick":rest) = parseArgs (cfg { cfgIterations = cfgIterations cfg `div` 10, cfgWarmup = cfgWarmup cfg `div` 10 }) rest
parseArgs cfg ("--json-out":path:rest) = parseArgs (cfg { cfgJsonOut = Just path }) rest
parseArgs cfg ("--verbose":rest) = parseArgs (cfg { cfgVerbose = True }) rest
parseArgs cfg (_:rest) = parseArgs cfg rest
