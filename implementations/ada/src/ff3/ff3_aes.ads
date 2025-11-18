-- FF3 AES Wrapper
-- Secure AES operations using UtilAda

with FF3_Types; use FF3_Types;

package FF3_AES is

   -- AES cipher context (opaque for security)
   type AES_Context is limited private;

   -- Security-critical result types
   type AES_Result is (Success, Invalid_Key_Length, Encryption_Failed, Decryption_Failed);

   -- Initialize AES context
   procedure Initialize_AES
     (Context    : out AES_Context;
      Key        : in Byte_Array;
      Result     : out AES_Result);

   -- Encrypt single AES block
   procedure Encrypt_Block
     (Context    : in AES_Context;
      Plaintext  : in AES_Block;
      Ciphertext : out AES_Block;
      Success    : out Boolean);

   -- Decrypt single AES block
   procedure Decrypt_Block
     (Context    : in AES_Context;
      Ciphertext : in AES_Block;
      Plaintext  : out AES_Block;
      Success    : out Boolean);

   -- Query functions
   function Is_Initialized (Context : AES_Context) return Boolean;

   function Get_Key_Size (Context : AES_Context) return Natural;

   -- Zero sensitive data
   procedure Secure_Clear (Context : in out AES_Context);

private

   -- Private AES context
   type AES_Context is limited record
      Key_Data     : Byte_Array (1 .. AES_256_Key_Size) := [others => 0];
      Key_Length   : Natural := 0;
      Initialized  : Boolean := False;
   end record;

end FF3_AES;