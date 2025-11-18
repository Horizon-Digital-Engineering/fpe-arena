{-|
Module      : FF3
Description : Pure functional FF3 Format Preserving Encryption library
Copyright   : (c) Horizon Digital Engineering, 2025
License     : BUSL-1.1
Maintainer  : engineering@horizondigital.dev
Stability   : experimental

FF3 - Main module re-exporting the pure functional FF3 FPE implementation.

⚠️ FF3 was withdrawn by NIST due to security vulnerabilities.
This implementation is for educational and research purposes only.

This library demonstrates functional programming principles applied to cryptography:
- Pure functions with no side effects
- Immutable data structures  
- Explicit error handling with Either types
- Referential transparency
- Type safety through Haskell's advanced type system
-}

module FF3
  ( -- * Re-exported Modules
    module FF3.API
  , module FF3.Alphabets
  , module FF3.Core
  ) where

import FF3.API
import FF3.Alphabets
import FF3.Core