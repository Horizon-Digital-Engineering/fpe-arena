-- FF3 AES Wrapper Implementation
-- Secure AES operations using UtilAda crypto library

with Util.Encoders.AES;
with Ada.Streams;

package body FF3_AES is

   -- Initialize AES context with key validation
   procedure Initialize_AES
     (Context    : out AES_Context;
      Key        : in Byte_Array;
      Result     : out AES_Result)
   is
   begin
      -- Validate key length
      if Key'Length /= AES_128_Key_Size and
         Key'Length /= AES_192_Key_Size and
         Key'Length /= AES_256_Key_Size then
         Result := Invalid_Key_Length;
         Context.Initialized := False;
         return;
      end if;

      -- Copy key to secure context
      Context.Key_Length := Key'Length;
      Context.Key_Data (1 .. Key'Length) := Key;

      -- Zero remaining key buffer for security
      if Key'Length < AES_256_Key_Size then
         Context.Key_Data (Key'Length + 1 .. AES_256_Key_Size) := [others => 0];
      end if;

      Context.Initialized := True;
      Result := Success;
   end Initialize_AES;

   -- Encrypt single AES block using UtilAda
   procedure Encrypt_Block
     (Context    : in AES_Context;
      Plaintext  : in AES_Block;
      Ciphertext : out AES_Block;
      Success    : out Boolean)
   is
   begin
      if not Context.Initialized then
         Success := False;
         Ciphertext := [others => 0];
         return;
      end if;

      declare
         use Ada.Streams;
         -- use Interfaces;  -- Not used

         -- Convert to stream element arrays for UtilAda
         -- Key_Stream : Stream_Element_Array (1 .. Stream_Element_Offset (Context.Key_Length)); -- Unused
         Plain_Stream : Stream_Element_Array (1 .. AES_Block_Size);
         Cipher_Stream : Stream_Element_Array (1 .. AES_Block_Size);

         -- Create secret key
         Key_String : String (1 .. Context.Key_Length);
         Encoder : Util.Encoders.AES.Encoder;
         Last, Encoded : Stream_Element_Offset;
      begin
         -- Convert key data to string
         for I in 1 .. Context.Key_Length loop
            Key_String (I) := Character'Val (Context.Key_Data (I));
         end loop;

         -- Copy plaintext
         for I in Plaintext'Range loop
            Plain_Stream (Stream_Element_Offset (I)) := Stream_Element (Plaintext (I));
         end loop;

         -- Set up encoder with ECB mode for single block
         Encoder.Set_Key (Util.Encoders.Create (Key_String), Util.Encoders.AES.ECB);

         -- Encrypt the block
         Encoder.Transform (Plain_Stream, Cipher_Stream, Last, Encoded);

         -- Convert result back to byte array
         for I in Ciphertext'Range loop
            Ciphertext (I) := Byte (Cipher_Stream (Stream_Element_Offset (I)));
         end loop;

         Success := True;

      exception
         when others =>
            Success := False;
            Ciphertext := [others => 0];
      end;
   end Encrypt_Block;

   -- Decrypt single AES block using UtilAda
   procedure Decrypt_Block
     (Context    : in AES_Context;
      Ciphertext : in AES_Block;
      Plaintext  : out AES_Block;
      Success    : out Boolean)
   is
   begin
      if not Context.Initialized then
         Success := False;
         Plaintext := [others => 0];
         return;
      end if;

      declare
         use Ada.Streams;
         -- use Interfaces;  -- Not used

         -- Convert to stream element arrays for UtilAda
         -- Key_Stream : Stream_Element_Array (1 .. Stream_Element_Offset (Context.Key_Length)); -- Unused
         Cipher_Stream : Stream_Element_Array (1 .. AES_Block_Size);
         Plain_Stream : Stream_Element_Array (1 .. AES_Block_Size);

         -- Create secret key
         Key_String : String (1 .. Context.Key_Length);
         Decoder : Util.Encoders.AES.Decoder;
         Last, Encoded : Stream_Element_Offset;
      begin
         -- Convert key data to string
         for I in 1 .. Context.Key_Length loop
            Key_String (I) := Character'Val (Context.Key_Data (I));
         end loop;

         -- Copy ciphertext
         for I in Ciphertext'Range loop
            Cipher_Stream (Stream_Element_Offset (I)) := Stream_Element (Ciphertext (I));
         end loop;

         -- Set up decoder with ECB mode for single block
         Decoder.Set_Key (Util.Encoders.Create (Key_String), Util.Encoders.AES.ECB);

         -- Decrypt the block
         Decoder.Transform (Cipher_Stream, Plain_Stream, Last, Encoded);

         -- Convert result back to byte array
         for I in Plaintext'Range loop
            Plaintext (I) := Byte (Plain_Stream (Stream_Element_Offset (I)));
         end loop;

         Success := True;

      exception
         when others =>
            Success := False;
            Plaintext := [others => 0];
      end;
   end Decrypt_Block;

   -- Security query functions
   function Is_Initialized (Context : AES_Context) return Boolean is
   begin
      return Context.Initialized;
   end Is_Initialized;

   function Get_Key_Size (Context : AES_Context) return Natural is
   begin
      return Context.Key_Length;
   end Get_Key_Size;

   -- Security procedure: securely zero all sensitive data
   procedure Secure_Clear (Context : in out AES_Context) is
   begin
      -- Securely zero the key material
      Context.Key_Data := [others => 0];
      Context.Key_Length := 0;
      Context.Initialized := False;
   end Secure_Clear;

end FF3_AES;