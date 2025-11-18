using System;
using Xunit;
using FF3.Core;

namespace FF3.Tests
{
    /// <summary>
    /// Tests for FF3Alphabets.cs - alphabet constant definitions
    /// </summary>
    public class FF3AlphabetsTests
    {
        [Fact]
        public void Digits_ShouldHaveCorrectLength()
        {
            Assert.Equal(10, FF3Alphabets.Digits.Length);
        }

        [Fact]
        public void Digits_ShouldContainCorrectCharacters()
        {
            Assert.Equal("0123456789", FF3Alphabets.Digits);
        }

        [Fact]
        public void HexLower_ShouldHaveCorrectLength()
        {
            Assert.Equal(16, FF3Alphabets.HexLower.Length);
        }

        [Fact]
        public void HexLower_ShouldContainCorrectCharacters()
        {
            Assert.Equal("0123456789abcdef", FF3Alphabets.HexLower);
        }

        [Fact]
        public void HexUpper_ShouldHaveCorrectLength()
        {
            Assert.Equal(16, FF3Alphabets.HexUpper.Length);
        }

        [Fact]
        public void HexUpper_ShouldContainCorrectCharacters()
        {
            Assert.Equal("0123456789ABCDEF", FF3Alphabets.HexUpper);
        }

        [Fact]
        public void Base36Lower_ShouldHaveCorrectLength()
        {
            Assert.Equal(36, FF3Alphabets.Base36Lower.Length);
        }

        [Fact]
        public void Base36Lower_ShouldContainCorrectCharacters()
        {
            Assert.Equal("0123456789abcdefghijklmnopqrstuvwxyz", FF3Alphabets.Base36Lower);
        }

        [Fact]
        public void Base36Upper_ShouldHaveCorrectLength()
        {
            Assert.Equal(36, FF3Alphabets.Base36Upper.Length);
        }

        [Fact]
        public void Base36Upper_ShouldContainCorrectCharacters()
        {
            Assert.Equal("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", FF3Alphabets.Base36Upper);
        }

        [Fact]
        public void Base62_ShouldHaveCorrectLength()
        {
            Assert.Equal(62, FF3Alphabets.Base62.Length);
        }

        [Fact]
        public void Base62_ShouldContainCorrectCharacters()
        {
            Assert.Equal("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", FF3Alphabets.Base62);
        }

        [Fact]
        public void Radix26_ShouldHaveCorrectLength()
        {
            Assert.Equal(26, FF3Alphabets.Radix26.Length);
        }

        [Fact]
        public void Radix26_ShouldContainCorrectCharacters()
        {
            Assert.Equal("0123456789abcdefghijklmnop", FF3Alphabets.Radix26);
        }

        [Fact]
        public void AllAlphabets_ShouldHaveUniqueCharacters()
        {
            // Each alphabet should not have duplicate characters
            Assert.Equal(FF3Alphabets.Digits.Length, FF3Alphabets.Digits.Distinct().Count());
            Assert.Equal(FF3Alphabets.HexLower.Length, FF3Alphabets.HexLower.Distinct().Count());
            Assert.Equal(FF3Alphabets.HexUpper.Length, FF3Alphabets.HexUpper.Distinct().Count());
            Assert.Equal(FF3Alphabets.Base36Lower.Length, FF3Alphabets.Base36Lower.Distinct().Count());
            Assert.Equal(FF3Alphabets.Base36Upper.Length, FF3Alphabets.Base36Upper.Distinct().Count());
            Assert.Equal(FF3Alphabets.Base62.Length, FF3Alphabets.Base62.Distinct().Count());
            Assert.Equal(FF3Alphabets.Radix26.Length, FF3Alphabets.Radix26.Distinct().Count());
        }

        [Fact]
        public void DigitsSubset_ShouldBeCorrect()
        {
            // Digits should be first 10 characters of Base36Lower
            Assert.Equal(FF3Alphabets.Digits, FF3Alphabets.Base36Lower[..10]);
        }

        [Fact]
        public void HexSubset_ShouldBeCorrect()
        {
            // HexLower should be first 16 characters of Base36Lower
            Assert.Equal(FF3Alphabets.HexLower, FF3Alphabets.Base36Lower[..16]);
        }

        [Fact]
        public void Base36Subset_ShouldBeCorrect()
        {
            // Base36Upper should be first 36 characters of Base62
            Assert.Equal(FF3Alphabets.Base36Upper, FF3Alphabets.Base62[..36]);
        }
    }
}