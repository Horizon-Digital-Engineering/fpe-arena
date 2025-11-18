-- FF3 Types and Constants
-- This package defines the core types for Format Preserving Encryption

package FF3_Types is
   pragma Pure;

   -- Maximum supported values for formal verification bounds
   Max_Radix        : constant := 62;   -- Maximum radix (0-9, A-Z, a-z)
   Min_Radix        : constant := 2;    -- Minimum radix (binary)
   Max_Text_Length  : constant := 64;   -- Maximum plaintext length for proofs
   Min_Text_Length  : constant := 2;    -- Minimum FF3 text length

   -- AES configuration constants
   AES_Block_Size   : constant := 16;   -- AES block size in bytes
   FF3_Tweak_Size   : constant := 8;    -- FF3 tweak size (64 bits)
   FF3_Rounds       : constant := 8;    -- FF3 Feistel rounds

   -- AES Key sizes
   AES_128_Key_Size : constant := 16;   -- AES-128 key size
   AES_192_Key_Size : constant := 24;   -- AES-192 key size
   AES_256_Key_Size : constant := 32;   -- AES-256 key size

   -- Basic types with formal constraints
   type Byte is mod 2**8;
   type Natural_Byte is range 0 .. 255;

   -- Radix type with formal bounds
   subtype Radix_Type is Natural range Min_Radix .. Max_Radix
     with Static_Predicate => Radix_Type >= Min_Radix and Radix_Type <= Max_Radix;

   -- Text length type with formal bounds
   subtype Text_Length_Type is Natural range Min_Text_Length .. Max_Text_Length
     with Static_Predicate => Text_Length_Type >= Min_Text_Length and
                             Text_Length_Type <= Max_Text_Length;

   -- Digit value constrained by radix
   subtype Digit_Value is Natural range 0 .. Max_Radix - 1;

   -- Round number for FF3 Feistel rounds
   subtype Round_Number is Natural range 0 .. FF3_Rounds - 1;

   -- Array types with formal bounds
   type Byte_Array is array (Positive range <>) of Byte;

   -- Fixed-size arrays for formal verification
   subtype AES_Block is Byte_Array (1 .. AES_Block_Size);
   subtype FF3_Tweak is Byte_Array (1 .. FF3_Tweak_Size);
   subtype FF3_W is Byte_Array (1 .. 4);  -- FF3 W value (half of tweak)

   -- AES key types with formal size constraints
   subtype AES_128_Key is Byte_Array (1 .. AES_128_Key_Size);
   subtype AES_192_Key is Byte_Array (1 .. AES_192_Key_Size);
   subtype AES_256_Key is Byte_Array (1 .. AES_256_Key_Size);

   -- Digit array for plaintext/ciphertext with bounded length
   type Digit_Array is array (Positive range <>) of Digit_Value;
   subtype Bounded_Digit_Array is Digit_Array (1 .. Max_Text_Length);

   -- Text representation using constrained string
   subtype Bounded_Text is String (1 .. Max_Text_Length);

   -- Formal verification predicates

   -- Predicate: Valid radix for digit value
   function Valid_Digit_For_Radix (Digit : Digit_Value; Radix : Radix_Type) return Boolean
   is (Digit < Natural (Radix))
   with Ghost,
        Global => null;

   -- Predicate: Valid digit array for radix
   function Valid_Digit_Array_For_Radix
     (Values : Digit_Array; Radix : Radix_Type) return Boolean
   with Ghost,
        Global => null;

   -- Predicate: Valid AES key size
   function Valid_AES_Key_Size (Key_Length : Natural) return Boolean
   is (Key_Length = AES_128_Key_Size or
       Key_Length = AES_192_Key_Size or
       Key_Length = AES_256_Key_Size)
   with Ghost,
        Global => null;

   -- Predicate: Valid text length for FF3
   function Valid_FF3_Text_Length (Length : Natural) return Boolean
   is (Length >= Min_Text_Length and Length <= Max_Text_Length)
   with Ghost,
        Global => null;

end FF3_Types;