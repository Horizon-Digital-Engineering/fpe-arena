{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import FF3 hiding (hex)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Random (mkStdGen, randomR, setStdGen, getStdRandom)
import Text.Printf (printf)

-- Options

data Options = Options
  { optIterations :: !Int
  , optAlphabets  :: ![String]
  , optMinLen     :: !Int
  , optMaxLen     :: !Int
  , optQuick      :: !Bool
  , optSeed       :: !(Maybe Int)
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
  { optIterations = 1000
  , optAlphabets  = ["digits", "hex-lower", "base36-lower", "base62"]
  , optMinLen     = 6
  , optMaxLen     = 20
  , optQuick      = False
  , optSeed       = Nothing
  }

-- CLI parsing ---------------------------------------------------------------

parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go opts [] = Right opts
    go _ ("-h":_) = Left usage
    go _ ("--help":_) = Left usage
    go opts ("--alphabets":val:rest) =
      let names = filter (not . null) $ map (map toLower . trim) (splitComma val)
      in if null names then Left "Error: --alphabets list cannot be empty"
                       else go (opts { optAlphabets = names }) rest
    go opts ("--min-length":val:rest) =
      case readEither val of
        Just n  -> go (opts { optMinLen = n }) rest
        Nothing -> Left "Error: invalid value for --min-length"
    go opts ("--max-length":val:rest) =
      case readEither val of
        Just n  -> go (opts { optMaxLen = n }) rest
        Nothing -> Left "Error: invalid value for --max-length"
    go opts ("--quick":rest) = go (opts { optQuick = True }) rest
    go opts ("--seed":val:rest) =
      case readEither val of
        Just n | n >= 0 -> go (opts { optSeed = Just n }) rest
        _ -> Left "Error: --seed must be non-negative"
    go opts (arg:rest)
      | "--" `prefixOf` arg = Left $ "Unknown option: " ++ arg
      | otherwise =
          case readEither arg of
            Just n | n > 0 -> go (opts { optIterations = n }) rest
            _              -> Left $ "Invalid iterations value: " ++ arg

prefixOf :: String -> String -> Bool
prefixOf pre str = pre == take (length pre) str

trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')

splitComma :: String -> [String]
splitComma [] = []
splitComma s  = case break (== ',') s of
  (chunk, [])     -> [chunk]
  (chunk, _:rest) -> chunk : splitComma rest

readEither :: Read a => String -> Maybe a
readEither str = case reads str of
  [(x, "")] -> Just x
  _          -> Nothing

usage :: String
usage = unlines
  [ "FF3 Stress Test Tool"
  , ""
  , "Usage: ff3-stresstest [OPTIONS] [ITERATIONS]"
  , ""
  , "Options:"
  , "  --alphabets LIST      Comma-separated list (default: digits,hex-lower,base36-lower,base62)"
  , "  --min-length N        Minimum plaintext length (default: 6)"
  , "  --max-length N        Maximum plaintext length (default: 20)"
  , "  --quick               Run 100 iterations (fast test)"
  , "  --seed N              Random seed for reproducibility"
  ]

-- Alphabet map --------------------------------------------------------------

type AlphabetInfo = (String, String, ByteString -> ByteString -> Either FF3Error FF3)

alphabetMap :: Map String AlphabetInfo
alphabetMap = Map.fromList
  [ ("digits",       ("digits",       "0123456789", digits))
  , ("hex-lower",    ("hex-lower",    "0123456789abcdef", hexLower))
  , ("hex-upper",    ("hex-upper",    "0123456789ABCDEF", hexUpper))
  , ("base36-lower", ("base36-lower", "0123456789abcdefghijklmnopqrstuvwxyz", base36Lower))
  , ("base36-upper", ("base36-upper", "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", base36Upper))
  , ("base62",       ("base62",       "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", base62))
  ]

-- Helpers -------------------------------------------------------------------

getRandomBytes :: Int -> IO ByteString
getRandomBytes n = BS.pack <$> replicateM' n (fromIntegral <$> randomRange 0 255)

randomRange :: Int -> Int -> IO Int
randomRange lo hi = getStdRandom (randomR (lo, hi))

replicateM' :: Int -> IO a -> IO [a]
replicateM' n action = sequence (replicate n action)

generatePlaintext :: String -> Int -> IO String
generatePlaintext alphabet len = do
  indices <- replicateM' len $ randomRange 0 (length alphabet - 1)
  pure $ map (alphabet !!) indices

-- Stress test loop ----------------------------------------------------------

stressAlphabet :: AlphabetInfo -> Options -> IO (Int, Int)
stressAlphabet (name, alphabetChars, factory) opts = do
  putStrLn $ "Testing " ++ name ++ "..."
  putStrLn $ "  Alphabet: " ++ alphabetChars ++ " (radix " ++ show (length alphabetChars) ++ ")"

  let total = optIterations opts
      interval = max 1 (total `div` 10)

  let loop 0 passed failed = pure (passed, failed)
      loop remaining passed failed = do
        key <- getRandomBytes 16
        tweak <- getRandomBytes 8
        len <- randomRange (optMinLen opts) (optMaxLen opts)
        plaintext <- generatePlaintext alphabetChars len
        case factory key tweak of
          Left err -> do
            printFailure key tweak plaintext "" Nothing (Just (show err))
            loop (remaining - 1) passed (failed + 1)
          Right cipher ->
            case encryptText cipher plaintext Nothing of
              Left err -> do
                printFailure key tweak plaintext "" Nothing (Just err)
                loop (remaining - 1) passed (failed + 1)
              Right ciphertext ->
                case decryptText cipher ciphertext Nothing of
                  Left err -> do
                    printFailure key tweak plaintext ciphertext Nothing (Just err)
                    loop (remaining - 1) passed (failed + 1)
                  Right decrypted ->
                    if decrypted == plaintext
                      then do
                        let completed = total - remaining + 1
                        when (completed `mod` interval == 0 || completed == total) $ do
                          let percent = completed * 100 `div` total
                          printf "  Progress: %d/%d (%d%%)\n" completed total percent
                        loop (remaining - 1) (passed + 1) failed
                      else do
                        printFailure key tweak plaintext ciphertext (Just decrypted) Nothing
                        loop (remaining - 1) passed (failed + 1)
  loop total 0 0

printFailure :: ByteString -> ByteString -> String -> String -> Maybe String -> Maybe String -> IO ()
printFailure key tweak plaintext ciphertext decrypted detail = do
  putStrLn "  Round-trip failed:"
  printf "    Key: %s\n" (hex key)
  printf "    Tweak: %s\n" (hex tweak)
  printf "    Plaintext: \"%s\"\n" plaintext
  unless (null ciphertext) $ printf "    Ciphertext: \"%s\"\n" ciphertext
  case decrypted of
    Just value -> printf "    Decrypted: \"%s\"\n" value
    Nothing    -> pure ()
  case detail of
    Just msg -> printf "    Detail: %s\n" msg
    Nothing  -> pure ()

hex :: ByteString -> String
hex = concatMap (printf "%02x") . BS.unpack

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left msg -> do
      putStrLn msg
      exitFailure
    Right opts0 -> do
      let opts = if optQuick opts0 then opts0 { optIterations = 100 } else opts0
      case validateOptions opts of
        Just err -> putStrLn err >> exitFailure
        Nothing  -> runWithOptions opts

validateOptions :: Options -> Maybe String
validateOptions opts
  | optIterations opts <= 0 = Just "iterations must be greater than 0"
  | optMinLen opts < 2      = Just "--min-length must be at least 2"
  | optMaxLen opts < optMinLen opts = Just "--max-length must be greater than or equal to --min-length"
  | otherwise = Nothing

runWithOptions :: Options -> IO ()
runWithOptions opts = do
  case optSeed opts of
    Just seed -> setStdGen (mkStdGen seed)
    Nothing   -> pure ()

  let alphInfos = map lookupAlphabet (optAlphabets opts)
  start <- getCurrentTime

  putStrLn "FF3 Stress Test v1.0"
  putStrLn "====================\n"
  putStrLn "Warning: FF3 was withdrawn by NIST; run for education and research only.\n"
  putStrLn "Test configuration"
  printf "  Iterations per alphabet: %d\n" (optIterations opts)
  putStrLn "  Random key/tweak generation: enabled"
  printf "  String length range: %d-%d characters\n" (optMinLen opts) (optMaxLen opts)
  putStrLn $ "  Alphabets: " ++ intercalate ", " (optAlphabets opts)
  putStrLn ""

  results <- mapM (`stressAlphabet` opts) alphInfos

  end <- getCurrentTime
  let totalTests    = sum [ optIterations opts | _ <- results ]
      totalFailures = sum [ f | (_, f) <- results ]
      durationMs    = floor (diffUTCTime end start * 1000) :: Int

  putStrLn "Summary"
  printf "  Total tests: %d\n" totalTests
  printf "  Failures: %d\n" totalFailures
  printf "  Duration: %d ms\n" durationMs
  when (durationMs > 0) $ do
    let throughput = fromIntegral totalTests * 1000 / fromIntegral durationMs :: Double
    printf "  Throughput: %.2f tests/sec\n" throughput

  if totalFailures == 0
    then do
      putStrLn "  Result: all stress tests passed"
      exitSuccess
    else do
      putStrLn "  Result: failures detected"
      exitFailure

lookupAlphabet :: String -> AlphabetInfo
lookupAlphabet name =
  fromMaybe (error ("Unknown alphabet: " ++ name)) (Map.lookup name alphabetMap)

-- Utility combinators -------------------------------------------------------

when :: Bool -> IO () -> IO ()
when predicate action = if predicate then action else pure ()

--------------------------------------------------------------------------------
