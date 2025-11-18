"""
FF3 Core - Pure cryptographic implementation of FF3 Format Preserving Encryption.
"""

import math
from typing import List, Optional
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes


class FF3Cipher:
    """Core FF3 cipher instance with pure cryptographic math."""
    
    def __init__(self, radix: int, key: bytes, tweak: bytes):
        """
        Initialize FF3 cipher with the given radix, encryption key, and tweak.
        Validates input parameters according to FF3 specification requirements.
        Creates the internal AES cipher with proper byte-reversal as required by FF3.
        Sets up maximum/minimum length constraints based on the radix value.
        """
        # Validate radix - relaxed for experimental repo
        if radix < 2 or radix > 62:
            raise ValueError(f"radix must be between 2 and 62, got {radix}")
        
        # Validate key length
        if len(key) not in [16, 24, 32]:
            raise ValueError(f"key length must be 16, 24, or 32 bytes, got {len(key)}")
        
        # Validate tweak length (FF3 requires exactly 64 bits = 8 bytes)
        if len(tweak) != 8:
            raise ValueError(f"tweak must be exactly 8 bytes (64 bits) for FF3, got {len(tweak)}")
        
        self.radix = radix
        self.key = key
        self.tweak = bytes(tweak)  # Copy to avoid external modifications
        
        # Create AES cipher with FF3 byte-reversal convention
        self.aes = self._create_aes_cipher(key)
        
        # Calculate maximum length for FF3 (more permissive for experimental use)
        if radix >= 2 and radix <= 36:
            self.max_len = 32  # For small radixes, allow reasonable lengths
        else:
            self.max_len = 56  # Conservative maximum for most practical use cases
        
        self.min_len = 2  # Minimum length for FF3
    
    def _create_aes_cipher(self, key: bytes):
        """
        Create AES cipher instance with FF3-specific byte-reversal convention.
        FF3 specification requires the encryption key to be byte-reversed before use.
        This ensures compatibility with the official NIST FF3 test vectors.
        Returns a configured AES cipher in ECB mode for use in round functions.
        """
        # FF3 specification requires byte reversal of the key
        reversed_key = bytes(reversed(key))
        cipher = Cipher(algorithms.AES(reversed_key), modes.ECB())
        return cipher
    
    def get_min_len(self) -> int:
        return self.min_len
    
    def get_max_len(self) -> int:
        return self.max_len
    
    def encrypt_digits(self, plaintext: List[int], additional_tweak: Optional[bytes] = None) -> List[int]:
        """
        Encrypt an array of digits using the FF3 Format Preserving Encryption algorithm.
        Takes integer digits within the specified radix and returns encrypted digits in same format.
        Optional additional_tweak parameter allows for domain separation or context-specific encryption.
        The output preserves the exact length and radix constraints of the input.
        """
        # Merge tweaks - main tweak + optional additional tweak
        effective_tweak = self.tweak
        if additional_tweak:
            # Simple XOR merge (8 bytes only)
            merged = bytearray(self.tweak)
            for i in range(min(len(additional_tweak), 8)):
                merged[i] ^= additional_tweak[i]
            effective_tweak = bytes(merged)
        
        return self._ff3_encrypt(plaintext, effective_tweak)
    
    def decrypt_digits(self, ciphertext: List[int], additional_tweak: Optional[bytes] = None) -> List[int]:
        """
        Decrypt an array of digits using the FF3 Format Preserving Encryption algorithm.
        Takes encrypted integer digits and returns the original plaintext digits in same format.
        Must use the same additional_tweak (if any) that was used during encryption.
        Performs the inverse of the encryption process through reverse Feistel rounds.
        """
        # Merge tweaks - main tweak + optional additional tweak
        effective_tweak = self.tweak
        if additional_tweak:
            # Simple XOR merge (8 bytes only)
            merged = bytearray(self.tweak)
            for i in range(min(len(additional_tweak), 8)):
                merged[i] ^= additional_tweak[i]
            effective_tweak = bytes(merged)
        
        return self._ff3_decrypt(ciphertext, effective_tweak)
    
    def _ff3_encrypt(self, plaintext: List[int], tweak: bytes) -> List[int]:
        """
        Core FF3 encryption implementation using 8-round Feistel network.
        Splits input into left/right halves and applies alternating round functions.
        Each round uses AES-based round function with proper digit reversal per NIST spec.
        Implements the exact algorithm from NIST SP 800-38G specification.
        """
        n = len(plaintext)
        
        # Split plaintext into left and right halves (u = ceil(n/2), v = n - u)
        u = (n + 1) // 2  # This gives ceil(n/2)
        v = n - u
        
        A = plaintext[:u].copy()
        B = plaintext[u:].copy()
        
        # Perform 8 Feistel rounds according to FF3 specification
        for i in range(8):
            if i % 2 == 0:
                # Even round: use B to update A
                W = self._calculate_w(tweak, i, B)
                P = self._calculate_p(i, W, B)
                m = self.radix ** u
                
                # FF3 uses reversed digit order: NUM_radix(REV(A))
                reversed_a = A[::-1]
                a_num = self._num_array_to_int(reversed_a)
                
                # c = (NUM_radix(REV(A)) + NUM(S)) mod radix^u
                y = (a_num + P) % m
                
                # C = REV(STR_radix(c))
                new_digits = self._int_to_num_array(y, u)
                A = new_digits[::-1]
            else:
                # Odd round: use A to update B
                W = self._calculate_w(tweak, i, A)
                P = self._calculate_p(i, W, A)
                m = self.radix ** v
                
                # FF3 uses reversed digit order: NUM_radix(REV(B))
                reversed_b = B[::-1]
                b_num = self._num_array_to_int(reversed_b)
                
                # c = (NUM_radix(REV(B)) + NUM(S)) mod radix^v
                y = (b_num + P) % m
                
                # C = REV(STR_radix(c))
                new_digits = self._int_to_num_array(y, v)
                B = new_digits[::-1]
        
        # Combine final A and B
        return A + B
    
    def _ff3_decrypt(self, ciphertext: List[int], tweak: bytes) -> List[int]:
        """
        Core FF3 decryption implementation using reverse 8-round Feistel network.
        Performs the exact inverse of encryption by running Feistel rounds in reverse order.
        Uses the same round function but with subtraction instead of addition.
        Restores the original plaintext from the ciphertext using the same key and tweak.
        """
        n = len(ciphertext)
        
        # Split ciphertext into left and right halves
        u = (n + 1) // 2
        v = n - u
        
        A = ciphertext[:u].copy()
        B = ciphertext[u:].copy()
        
        # Perform 8 Feistel rounds in reverse order
        for i in range(7, -1, -1):
            if i % 2 == 0:
                # Even round: use B to update A (reverse)
                W = self._calculate_w(tweak, i, B)
                P = self._calculate_p(i, W, B)
                m = self.radix ** u
                
                # FF3 uses reversed digit order: NUM_radix(REV(A))
                reversed_a = A[::-1]
                a_num = self._num_array_to_int(reversed_a)
                
                # c = (NUM_radix(REV(A)) - NUM(S)) mod radix^u
                y = (a_num - P) % m
                
                # C = REV(STR_radix(c))
                new_digits = self._int_to_num_array(y, u)
                A = new_digits[::-1]
            else:
                # Odd round: use A to update B (reverse)
                W = self._calculate_w(tweak, i, A)
                P = self._calculate_p(i, W, A)
                m = self.radix ** v
                
                # FF3 uses reversed digit order: NUM_radix(REV(B))
                reversed_b = B[::-1]
                b_num = self._num_array_to_int(reversed_b)
                
                # c = (NUM_radix(REV(B)) - NUM(S)) mod radix^v
                y = (b_num - P) % m
                
                # C = REV(STR_radix(c))
                new_digits = self._int_to_num_array(y, v)
                B = new_digits[::-1]
        
        # Combine final A and B
        return A + B
    
    def _calculate_w(self, tweak: bytes, round_num: int, block: List[int]) -> bytes:
        """
        Calculate the W parameter for FF3 round function according to NIST specification.
        Splits the 8-byte tweak into left (Tl) and right (Tr) 4-byte halves.
        Uses Tr for even rounds and Tl for odd rounds as required by FF3.
        This ensures proper tweak utilization across all 8 Feistel rounds.
        """
        # NIST FF3 W calculation: split 8-byte tweak into Tl (first 4 bytes) and Tr (last 4 bytes)
        w = bytearray(4)
        
        if round_num % 2 == 0:
            # Even rounds: W = Tr (rightmost 4 bytes)
            w[:] = tweak[4:8]
        else:
            # Odd rounds: W = Tl (leftmost 4 bytes)
            w[:] = tweak[:4]
        
        return bytes(w)
    
    def _calculate_p(self, round_num: int, w: bytes, block: List[int]) -> int:
        """
        Calculate the P value for FF3 round function using AES encryption.
        Constructs 16-byte input from W XOR round_number and reversed block digits.
        Applies FF3 byte-reversal before and after AES encryption as per NIST spec.
        Returns the encrypted result as an integer for use in Feistel round arithmetic.
        """
        # NIST FF3 P calculation with proper byte reversal
        # P = REVB(AES-ECB(K, REVB(W XOR [i]_4 || NUM_radix(REV(B)))))
        
        # Create 16-byte input
        input_data = bytearray(16)
        
        # First 4 bytes: W XOR with round number in the last byte of W
        input_data[:4] = w[:4]
        input_data[3] ^= round_num
        
        # Last 12 bytes: NUM_radix(REV(B)) - reverse digits of B first
        reversed_block = block[::-1]
        block_num = self._num_array_to_int(reversed_block)
        
        # Convert to bytes with proper padding to 12 bytes
        if block_num == 0:
            block_bytes = b'\x00' * 12
        else:
            block_bytes_raw = block_num.to_bytes((block_num.bit_length() + 7) // 8, byteorder='big')
            # Pad to 12 bytes (big-endian)
            block_bytes = b'\x00' * (12 - len(block_bytes_raw)) + block_bytes_raw
        
        input_data[4:] = block_bytes
        
        # Apply FF3 byte reversal convention: REVB before AES
        reversed_input = bytes(reversed(input_data))
        
        # Encrypt with AES
        encryptor = self.aes.encryptor()
        aes_output = encryptor.update(reversed_input)
        
        # Apply FF3 byte reversal convention: REVB after AES
        output = bytes(reversed(aes_output))
        
        # Convert to integer
        p = int.from_bytes(output, byteorder='big')
        return p
    
    def _num_array_to_int(self, digits: List[int]) -> int:
        """
        Convert an array of digits to a single integer using the cipher's radix.
        Treats the digit array as a number in the specified radix base.
        Most significant digit is first in the array (big-endian representation).
        Used to convert digit blocks to integers for arithmetic operations in Feistel rounds.
        """
        result = 0
        for digit in digits:
            result = result * self.radix + digit
        return result
    
    def _int_to_num_array(self, num: int, length: int) -> List[int]:
        """
        Convert an integer to a digit array of specified length using the cipher's radix.
        Performs base conversion and pads with leading zeros to reach target length.
        Returns digits in big-endian order (most significant digit first).
        Used to convert Feistel round results back to digit arrays for further processing.
        """
        if num == 0:
            return [0] * length
        
        digits = []
        while num > 0:
            digits.append(num % self.radix)
            num //= self.radix
        
        # Pad with zeros to required length
        while len(digits) < length:
            digits.append(0)
        
        # Reverse to get most significant digit first
        return digits[::-1]