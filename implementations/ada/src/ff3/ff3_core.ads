-- FF3 Cipher Package
-- Format Preserving Encryption

with FF3_Types;  use FF3_Types;
with FF3_BigInt; use FF3_BigInt;
with FF3_AES;

package FF3_Core is

   -- FF3 Cipher type (opaque for security)
   type FF3_Cipher is limited private;

   -- Initialization result type
   type Init_Result is (Success, Invalid_Radix, Invalid_Key_Length, Invalid_Tweak_Length);

   -- Constructor
   procedure Initialize_Cipher
     (Cipher     : out FF3_Cipher;
      Radix      : in Radix_Type;
      Key        : in Byte_Array;
      Tweak      : in FF3_Tweak;
      Result     : out Init_Result);

   -- Core encryption
   procedure Encrypt_Digits
     (Cipher     : in FF3_Cipher;
      Plaintext  : in Digit_Array;
      Ciphertext : out Digit_Array;
      Success    : out Boolean);

   -- Core decryption
   procedure Decrypt_Digits
     (Cipher     : in FF3_Cipher;
      Ciphertext : in Digit_Array;
      Plaintext  : out Digit_Array;
      Success    : out Boolean);

   -- Query functions
   function Get_Radix (Cipher : FF3_Cipher) return Radix_Type;

   function Is_Initialized (Cipher : FF3_Cipher) return Boolean;

   -- FF3 algorithm components

   -- Reverse digit array
   function Reverse_Digits (Values : Digit_Array) return Digit_Array;

   -- Calculate W value for FF3 round
   procedure Calculate_W
     (Cipher : in FF3_Cipher;
      Tweak  : in FF3_Tweak;
      Round  : in Round_Number;
      Half   : in Digit_Array;
      W      : out FF3_W;
      Success : out Boolean);

   -- Calculate P value for FF3 round
   procedure Calculate_P
     (Cipher  : in FF3_Cipher;
      Round   : in Round_Number;
      W       : in FF3_W;
      Half    : in Digit_Array;
      P       : out Big_Integer;
      Success : out Boolean);

   -- Calculate modulus for FF3 round
   function Calculate_Modulus
     (Cipher : FF3_Cipher;
      Length : Text_Length_Type) return Big_Integer;

   -- Helper function for byte array conversion
   function From_Byte_Array (Bytes : Byte_Array) return Big_Integer;

private

   -- Private cipher representation
   type FF3_Cipher is limited record
      Radix        : Radix_Type := Min_Radix;
      Key          : Byte_Array (1 .. AES_256_Key_Size) := [others => 0];
      Key_Length   : Natural := 0;
      Tweak        : FF3_Tweak := [others => 0];
      AES_Context  : FF3_AES.AES_Context;
      Initialized  : Boolean := False;
   end record;

end FF3_Core;