-- FF3 API - High-level String Interface
-- This package provides convenient string-based FF3 operations

with FF3_Types;      use FF3_Types;
with FF3_Core;       use FF3_Core;
with FF3_Alphabets;  use FF3_Alphabets;

package FF3_API is

   -- High-level cipher type with alphabet support
   type FF3_String_Cipher is limited private;

   -- Result type for string operations
   type String_Result is (Success, Invalid_Alphabet, Invalid_Text, Cipher_Error);

   -- Factory functions for common alphabets
   procedure Create_Digits_Cipher
     (Cipher : out FF3_String_Cipher;
      Key    : in Byte_Array;
      Tweak  : in FF3_Tweak;
      Result : out String_Result);

   procedure Create_Hex_Lower_Cipher
     (Cipher : out FF3_String_Cipher;
      Key    : in Byte_Array;
      Tweak  : in FF3_Tweak;
      Result : out String_Result);

   procedure Create_Hex_Upper_Cipher
     (Cipher : out FF3_String_Cipher;
      Key    : in Byte_Array;
      Tweak  : in FF3_Tweak;
      Result : out String_Result);

   procedure Create_Base36_Lower_Cipher
     (Cipher : out FF3_String_Cipher;
      Key    : in Byte_Array;
      Tweak  : in FF3_Tweak;
      Result : out String_Result);

   procedure Create_Base36_Upper_Cipher
     (Cipher : out FF3_String_Cipher;
      Key    : in Byte_Array;
      Tweak  : in FF3_Tweak;
      Result : out String_Result);

   procedure Create_Base62_Cipher
     (Cipher : out FF3_String_Cipher;
      Key    : in Byte_Array;
      Tweak  : in FF3_Tweak;
      Result : out String_Result);

   -- Custom alphabet cipher
   procedure Create_Custom_Cipher
     (Cipher   : out FF3_String_Cipher;
      Key      : in Byte_Array;
      Tweak    : in FF3_Tweak;
      Alphabet : in Alphabet_Spec;
      Result   : out String_Result);

   -- String encryption/decryption operations
   procedure Encrypt_String
     (Cipher     : in FF3_String_Cipher;
      Plaintext  : in String;
      Ciphertext : out String;
      Result     : out String_Result);

   procedure Decrypt_String
     (Cipher     : in FF3_String_Cipher;
      Ciphertext : in String;
      Plaintext  : out String;
      Result     : out String_Result);

   -- Query functions
   function Get_Radix (Cipher : FF3_String_Cipher) return Radix_Type;

   function Is_Initialized (Cipher : FF3_String_Cipher) return Boolean;

   -- Utility functions for testing and validation
   function Valid_Text_For_Cipher (Cipher : FF3_String_Cipher; Text : String) return Boolean;

private

   type FF3_String_Cipher is limited record
      Core_Cipher : FF3_Core.FF3_Cipher;
      Alphabet    : Alphabet_Spec;
      Initialized : Boolean := False;
   end record;

end FF3_API;