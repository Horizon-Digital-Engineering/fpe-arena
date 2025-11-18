-- FF3 API Implementation - String Interface
-- High-level operations with automatic string↔digit conversion

package body FF3_API is

   procedure Create_Digits_Cipher
     (Cipher : out FF3_String_Cipher;
      Key    : in Byte_Array;
      Tweak  : in FF3_Tweak;
      Result : out String_Result) is
   begin
      Create_Custom_Cipher (Cipher, Key, Tweak, Digits_Spec, Result);
   end Create_Digits_Cipher;

   procedure Create_Hex_Lower_Cipher
     (Cipher : out FF3_String_Cipher;
      Key    : in Byte_Array;
      Tweak  : in FF3_Tweak;
      Result : out String_Result) is
   begin
      Create_Custom_Cipher (Cipher, Key, Tweak, Hex_Lower_Spec, Result);
   end Create_Hex_Lower_Cipher;

   procedure Create_Hex_Upper_Cipher
     (Cipher : out FF3_String_Cipher;
      Key    : in Byte_Array;
      Tweak  : in FF3_Tweak;
      Result : out String_Result) is
   begin
      Create_Custom_Cipher (Cipher, Key, Tweak, Hex_Upper_Spec, Result);
   end Create_Hex_Upper_Cipher;

   procedure Create_Base36_Lower_Cipher
     (Cipher : out FF3_String_Cipher;
      Key    : in Byte_Array;
      Tweak  : in FF3_Tweak;
      Result : out String_Result) is
   begin
      Create_Custom_Cipher (Cipher, Key, Tweak, Base36_Lower_Spec, Result);
   end Create_Base36_Lower_Cipher;

   procedure Create_Base36_Upper_Cipher
     (Cipher : out FF3_String_Cipher;
      Key    : in Byte_Array;
      Tweak  : in FF3_Tweak;
      Result : out String_Result) is
   begin
      Create_Custom_Cipher (Cipher, Key, Tweak, Base36_Upper_Spec, Result);
   end Create_Base36_Upper_Cipher;

   procedure Create_Base62_Cipher
     (Cipher : out FF3_String_Cipher;
      Key    : in Byte_Array;
      Tweak  : in FF3_Tweak;
      Result : out String_Result) is
   begin
      Create_Custom_Cipher (Cipher, Key, Tweak, Base62_Spec, Result);
   end Create_Base62_Cipher;

   procedure Create_Custom_Cipher
     (Cipher   : out FF3_String_Cipher;
      Key      : in Byte_Array;
      Tweak    : in FF3_Tweak;
      Alphabet : in Alphabet_Spec;
      Result   : out String_Result) is
      Init_Result : FF3_Core.Init_Result;
   begin
      -- Initialize core cipher
      FF3_Core.Initialize_Cipher (Cipher.Core_Cipher, Alphabet.Length, Key, Tweak, Init_Result);

      case Init_Result is
         when FF3_Core.Success =>
            Cipher.Alphabet := Alphabet;
            Cipher.Initialized := True;
            Result := Success;

         when FF3_Core.Invalid_Radix =>
            Result := Invalid_Alphabet;
            Cipher.Initialized := False;

         when FF3_Core.Invalid_Key_Length =>
            Result := Cipher_Error;
            Cipher.Initialized := False;

         when FF3_Core.Invalid_Tweak_Length =>
            Result := Cipher_Error;
            Cipher.Initialized := False;
      end case;
   end Create_Custom_Cipher;

   procedure Encrypt_String
     (Cipher     : in FF3_String_Cipher;
      Plaintext  : in String;
      Ciphertext : out String;
      Result     : out String_Result) is
      Plaintext_Digits  : Digit_Array (1 .. Plaintext'Length);
      Ciphertext_Digits : Digit_Array (1 .. Plaintext'Length);
      String_Success    : Boolean;
      Encrypt_Success   : Boolean;
   begin
      if not Cipher.Initialized then
         Result := Cipher_Error;
         return;
      end if;

      -- Convert string to digits
      String_To_Digit_Array (Plaintext, Cipher.Alphabet, Plaintext_Digits, String_Success);
      if not String_Success then
         Result := Invalid_Text;
         return;
      end if;

      -- Encrypt digits
      FF3_Core.Encrypt_Digits (Cipher.Core_Cipher, Plaintext_Digits, Ciphertext_Digits, Encrypt_Success);
      if not Encrypt_Success then
         Result := Cipher_Error;
         return;
      end if;

      -- Convert digits back to string
      Digit_Array_To_String (Ciphertext_Digits, Cipher.Alphabet, Ciphertext, String_Success);
      if not String_Success then
         Result := Cipher_Error;
         return;
      end if;

      Result := Success;
   end Encrypt_String;

   procedure Decrypt_String
     (Cipher     : in FF3_String_Cipher;
      Ciphertext : in String;
      Plaintext  : out String;
      Result     : out String_Result) is
      Ciphertext_Digits : Digit_Array (1 .. Ciphertext'Length);
      Plaintext_Digits  : Digit_Array (1 .. Ciphertext'Length);
      String_Success    : Boolean;
      Decrypt_Success   : Boolean;
   begin
      if not Cipher.Initialized then
         Result := Cipher_Error;
         return;
      end if;

      -- Convert string to digits
      String_To_Digit_Array (Ciphertext, Cipher.Alphabet, Ciphertext_Digits, String_Success);
      if not String_Success then
         Result := Invalid_Text;
         return;
      end if;

      -- Decrypt digits
      FF3_Core.Decrypt_Digits (Cipher.Core_Cipher, Ciphertext_Digits, Plaintext_Digits, Decrypt_Success);
      if not Decrypt_Success then
         Result := Cipher_Error;
         return;
      end if;

      -- Convert digits back to string
      Digit_Array_To_String (Plaintext_Digits, Cipher.Alphabet, Plaintext, String_Success);
      if not String_Success then
         Result := Cipher_Error;
         return;
      end if;

      Result := Success;
   end Decrypt_String;

   function Get_Radix (Cipher : FF3_String_Cipher) return Radix_Type is
   begin
      if not Cipher.Initialized then
         return Min_Radix;
      end if;
      return FF3_Core.Get_Radix (Cipher.Core_Cipher);
   end Get_Radix;

   function Is_Initialized (Cipher : FF3_String_Cipher) return Boolean is
   begin
      return Cipher.Initialized and FF3_Core.Is_Initialized (Cipher.Core_Cipher);
   end Is_Initialized;

   function Valid_Text_For_Cipher (Cipher : FF3_String_Cipher; Text : String) return Boolean is
   begin
      if not Cipher.Initialized then
         return False;
      end if;
      return Valid_String (Text, Cipher.Alphabet);
   end Valid_Text_For_Cipher;

end FF3_API;