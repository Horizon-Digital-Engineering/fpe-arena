namespace FF3.Core
{
    /// <summary>
    /// Predefined alphabet character sets for FF3 Format Preserving Encryption
    /// </summary>
    public static class FF3Alphabets
    {
        /// <summary>
        /// Digits 0-9 (radix 10)
        /// </summary>
        public const string Digits = "0123456789";

        /// <summary>
        /// Lowercase hexadecimal 0-9a-f (radix 16)
        /// </summary>
        public const string HexLower = "0123456789abcdef";

        /// <summary>
        /// Uppercase hexadecimal 0-9A-F (radix 16)
        /// </summary>
        public const string HexUpper = "0123456789ABCDEF";

        /// <summary>
        /// Base36 lowercase 0-9a-z (radix 36)
        /// </summary>
        public const string Base36Lower = "0123456789abcdefghijklmnopqrstuvwxyz";

        /// <summary>
        /// Base36 uppercase 0-9A-Z (radix 36)
        /// </summary>
        public const string Base36Upper = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

        /// <summary>
        /// Base62 alphanumeric 0-9A-Za-z (radix 62)
        /// </summary>
        public const string Base62 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

        /// <summary>
        /// Radix26 0-9a-p (radix 26)
        /// </summary>
        public const string Radix26 = "0123456789abcdefghijklmnop";
    }
}