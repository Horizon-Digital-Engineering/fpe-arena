"""
FF3 Format Preserving Encryption - Python Implementation
Experimental implementation for research and educational purposes.

⚠️ FF3 was withdrawn by NIST due to security vulnerabilities.
This implementation is for educational and research purposes only.
"""

from .ff3_api import (
    FF3,
    from_spec,
    digits,
    hex,
    hex_lower,
    hex_upper,
    base36,
    base36_lower,
    base36_upper,
    base62,
)

from .ff3_alphabets import (
    AlphabetSpec,
    ALPHA_DIGITS,
    ALPHA_HEX_LOWER,
    ALPHA_HEX_UPPER,
    ALPHA_BASE36_LOW,
    ALPHA_BASE36_UP,
    ALPHA_BASE62,
    SPEC_DIGITS,
    SPEC_HEX_LOWER,
    SPEC_HEX_UPPER,
    SPEC_BASE36_LOW,
    SPEC_BASE36_UP,
    SPEC_BASE62,
)

__version__ = "0.1.0"
__all__ = [
    # Core classes
    "FF3",
    "AlphabetSpec",

    # Constructor functions
    "from_spec",
    "digits",
    "hex",
    "hex_lower",
    "hex_upper",
    "base36",
    "base36_lower",
    "base36_upper",
    "base62",

    # Alphabet constants
    "ALPHA_DIGITS",
    "ALPHA_HEX_LOWER",
    "ALPHA_HEX_UPPER",
    "ALPHA_BASE36_LOW",
    "ALPHA_BASE36_UP",
    "ALPHA_BASE62",
    "SPEC_DIGITS",
    "SPEC_HEX_LOWER",
    "SPEC_HEX_UPPER",
    "SPEC_BASE36_LOW",
    "SPEC_BASE36_UP",
    "SPEC_BASE62",
]