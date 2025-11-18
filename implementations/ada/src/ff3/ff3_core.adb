-- FF3 Cipher Implementation with 8-Round Feistel Network
-- Format Preserving Encryption

with FF3_AES;    use FF3_AES;

package body FF3_Core is

   -- Initialize FF3 cipher with comprehensive validation
   procedure Initialize_Cipher
     (Cipher     : out FF3_Cipher;
      Radix      : in Radix_Type;
      Key        : in Byte_Array;
      Tweak      : in FF3_Tweak;
      Result     : out Init_Result)
   is
      AES_Status : FF3_AES.AES_Result;
   begin
      -- Validate parameters
      if Key'Length /= AES_128_Key_Size and
         Key'Length /= AES_192_Key_Size and
         Key'Length /= AES_256_Key_Size then
         Result := Invalid_Key_Length;
         Cipher.Initialized := False;
         return;
      end if;

      if Radix < Min_Radix or Radix > Max_Radix then
         Result := Invalid_Radix;
         Cipher.Initialized := False;
         return;
      end if;

      -- Initialize AES context with REVERSED key (FF3 requirement)
      declare
         Reversed_Key : Byte_Array (1 .. Key'Length);
      begin
         -- Reverse the key as required by FF3 specification
         for I in Key'Range loop
            Reversed_Key (I) := Key (Key'Last - I + Key'First);
         end loop;

         Initialize_AES (Cipher.AES_Context, Reversed_Key, AES_Status);
      end;
      if AES_Status /= FF3_AES.Success then
         Result := Invalid_Key_Length;
         Cipher.Initialized := False;
         return;
      end if;

      -- Store cipher parameters
      Cipher.Radix := Radix;
      Cipher.Key_Length := Key'Length;
      Cipher.Key (1 .. Key'Length) := Key;
      Cipher.Tweak := Tweak;
      Cipher.Initialized := True;

      Result := Success;
   end Initialize_Cipher;

   -- Core FF3 encryption with 8-round Feistel network
   procedure Encrypt_Digits
     (Cipher     : in FF3_Cipher;
      Plaintext  : in Digit_Array;
      Ciphertext : out Digit_Array;
      Success    : out Boolean)
   is
      N : constant Natural := Plaintext'Length;
      U : constant Natural := (N + 1) / 2;  -- ceil(N/2)
      V : constant Natural := N - U;        -- floor(N/2)

      -- Left and right halves
      A : Digit_Array (1 .. U);
      B : Digit_Array (1 .. V);

      -- Working variables for Feistel rounds
      W : FF3_W;
      P : Big_Integer;
      Y : Big_Integer;
      Modulus : Big_Integer;
      Temp_Digits : Digit_Array (1 .. Natural'Max (U, V));
      AES_Success : Boolean := True;
   begin
      Success := False;

      if not Cipher.Initialized then
         return;
      end if;

      -- Split plaintext into halves
      A := Plaintext (Plaintext'First .. Plaintext'First + U - 1);
      if V > 0 then
         B := Plaintext (Plaintext'First + U .. Plaintext'Last);
      end if;

      -- 8-round Feistel network with formal loop invariant
      for Round in 0 .. 7 loop
         pragma Loop_Invariant (Round >= 0 and Round <= 7);
         pragma Loop_Invariant (A'Length = U and B'Length = V);
         pragma Loop_Invariant (Valid_Digit_Array_For_Radix (A, Cipher.Radix));
         pragma Loop_Invariant (Valid_Digit_Array_For_Radix (B, Cipher.Radix));

         if Round mod 2 = 0 then
            -- Even round: B affects A
            -- Calculate W = AES_Encrypt(Tweak XOR Round)
            Calculate_W (Cipher, Cipher.Tweak, Round_Number (Round), B, W, AES_Success);
            exit when not AES_Success;

            -- Calculate P from W and B
            Calculate_P (Cipher, Round_Number (Round), W, B, P, AES_Success);
            exit when not AES_Success;

            -- Calculate modulus = radix^u
            Modulus := Calculate_Modulus (Cipher, Text_Length_Type (U));

            -- Y = (NUM(REV(A)) + P) mod radix^u
            declare
               Reversed_A : Digit_Array := Reverse_Digits (A);
               A_Big : Big_Integer := From_Digit_Array (Reversed_A, Cipher.Radix);
            begin
               Y := Mod_Add (A_Big, P, Modulus);

               -- A = REV(STR_radix(Y))
               Temp_Digits (1 .. U) := To_Digit_Array (Y, Cipher.Radix, Text_Length_Type (U));
               A := Reverse_Digits (Temp_Digits (1 .. U));
            end;

         else
            -- Odd round: A affects B
            -- Calculate W = AES_Encrypt(Tweak XOR Round)
            Calculate_W (Cipher, Cipher.Tweak, Round_Number (Round), A, W, AES_Success);
            exit when not AES_Success;

            -- Calculate P from W and A
            Calculate_P (Cipher, Round_Number (Round), W, A, P, AES_Success);
            exit when not AES_Success;

            -- Calculate modulus = radix^v
            if V > 0 then
               Modulus := Calculate_Modulus (Cipher, Text_Length_Type (V));

               -- Y = (NUM(REV(B)) + P) mod radix^v
               declare
                  Reversed_B : Digit_Array := Reverse_Digits (B);
                  B_Big : Big_Integer := From_Digit_Array (Reversed_B, Cipher.Radix);
               begin
                  Y := Mod_Add (B_Big, P, Modulus);

                  -- B = REV(STR_radix(Y))
                  Temp_Digits (1 .. V) := To_Digit_Array (Y, Cipher.Radix, Text_Length_Type (V));
                  B := Reverse_Digits (Temp_Digits (1 .. V));
               end;
            end if;
         end if;
      end loop;

      -- Combine results A || B
      Ciphertext (Ciphertext'First .. Ciphertext'First + U - 1) := A;
      if V > 0 then
         Ciphertext (Ciphertext'First + U .. Ciphertext'Last) := B;
      end if;

      Success := AES_Success;
   end Encrypt_Digits;

   -- Core FF3 decryption (inverse of encryption)
   procedure Decrypt_Digits
     (Cipher     : in FF3_Cipher;
      Ciphertext : in Digit_Array;
      Plaintext  : out Digit_Array;
      Success    : out Boolean)
   is
      N : constant Natural := Ciphertext'Length;
      U : constant Natural := (N + 1) / 2;  -- ceil(N/2)
      V : constant Natural := N - U;        -- floor(N/2)

      -- Left and right halves
      A : Digit_Array (1 .. U);
      B : Digit_Array (1 .. V);

      -- Working variables for reverse Feistel rounds
      W : FF3_W;
      P : Big_Integer;
      Y : Big_Integer;
      Modulus : Big_Integer;
      Temp_Digits : Digit_Array (1 .. Natural'Max (U, V));
      AES_Success : Boolean := True;
   begin
      Success := False;

      if not Cipher.Initialized then
         return;
      end if;

      -- Split ciphertext into halves
      A := Ciphertext (Ciphertext'First .. Ciphertext'First + U - 1);
      if V > 0 then
         B := Ciphertext (Ciphertext'First + U .. Ciphertext'Last);
      end if;

      -- 8-round Feistel network in REVERSE order
      for Round in reverse 0 .. 7 loop
         pragma Loop_Invariant (Round >= 0 and Round <= 7);
         pragma Loop_Invariant (A'Length = U and B'Length = V);
         pragma Loop_Invariant (Valid_Digit_Array_For_Radix (A, Cipher.Radix));
         pragma Loop_Invariant (Valid_Digit_Array_For_Radix (B, Cipher.Radix));

         if Round mod 2 = 0 then
            -- Even round: B affects A (same as encryption)
            Calculate_W (Cipher, Cipher.Tweak, Round_Number (Round), B, W, AES_Success);
            exit when not AES_Success;

            Calculate_P (Cipher, Round_Number (Round), W, B, P, AES_Success);
            exit when not AES_Success;

            -- Calculate modulus = radix^u
            Modulus := Calculate_Modulus (Cipher, Text_Length_Type (U));

            -- Y = (NUM(REV(A)) - P) mod radix^u
            declare
               Reversed_A : Digit_Array := Reverse_Digits (A);
               A_Big : Big_Integer := From_Digit_Array (Reversed_A, Cipher.Radix);
            begin
               Y := Mod_Subtract (A_Big, P, Modulus);

               -- A = REV(STR_radix(Y))
               Temp_Digits (1 .. U) := To_Digit_Array (Y, Cipher.Radix, Text_Length_Type (U));
               A := Reverse_Digits (Temp_Digits (1 .. U));
            end;

         else
            -- Odd round: A affects B (same as encryption)
            Calculate_W (Cipher, Cipher.Tweak, Round_Number (Round), A, W, AES_Success);
            exit when not AES_Success;

            Calculate_P (Cipher, Round_Number (Round), W, A, P, AES_Success);
            exit when not AES_Success;

            -- Calculate modulus = radix^v
            if V > 0 then
               Modulus := Calculate_Modulus (Cipher, Text_Length_Type (V));

               -- Y = (NUM(REV(B)) - P) mod radix^v
               declare
                  Reversed_B : Digit_Array := Reverse_Digits (B);
                  B_Big : Big_Integer := From_Digit_Array (Reversed_B, Cipher.Radix);
               begin
                  Y := Mod_Subtract (B_Big, P, Modulus);

                  -- B = REV(STR_radix(Y))
                  Temp_Digits (1 .. V) := To_Digit_Array (Y, Cipher.Radix, Text_Length_Type (V));
                  B := Reverse_Digits (Temp_Digits (1 .. V));
               end;
            end if;
         end if;
      end loop;

      -- Combine results A || B
      Plaintext (Plaintext'First .. Plaintext'First + U - 1) := A;
      if V > 0 then
         Plaintext (Plaintext'First + U .. Plaintext'Last) := B;
      end if;

      Success := AES_Success;
   end Decrypt_Digits;

   -- Query functions
   function Get_Radix (Cipher : FF3_Cipher) return Radix_Type is
   begin
      return Cipher.Radix;
   end Get_Radix;

   function Is_Initialized (Cipher : FF3_Cipher) return Boolean is
   begin
      return Cipher.Initialized;
   end Is_Initialized;

   -- FF3 helper functions with formal contracts

   function Reverse_Digits (Values : Digit_Array) return Digit_Array is
      Result : Digit_Array (Values'Range);
   begin
      for I in Values'Range loop
         pragma Loop_Invariant (I in Values'Range);
         Result (I) := Values (Values'Last - I + Values'First);
      end loop;
      return Result;
   end Reverse_Digits;

   -- Reverse byte array (FF3 REVB byte reversal convention)
   function Reverse_Bytes (Values : AES_Block) return AES_Block is
      Result : AES_Block;
   begin
      for I in Values'Range loop
         Result (I) := Values (Values'Last - I + Values'First);
      end loop;
      return Result;
   end Reverse_Bytes;

   -- Calculate W value for FF3 round function (NIST FF3 specification)
   procedure Calculate_W
     (Cipher : in FF3_Cipher;
      Tweak  : in FF3_Tweak;
      Round  : in Round_Number;
      Half   : in Digit_Array;
      W      : out FF3_W;
      Success : out Boolean)
   is
   begin
      -- NIST FF3 W calculation: split 8-byte tweak into Tl (first 4 bytes) and Tr (last 4 bytes)
      if Round mod 2 = 0 then
         -- Even rounds: W = Tr (rightmost 4 bytes)
         W := Tweak (5 .. 8);
      else
         -- Odd rounds: W = Tl (leftmost 4 bytes)
         W := Tweak (1 .. 4);
      end if;

      Success := True;
   end Calculate_W;

   -- Calculate P value for FF3 round function
   procedure Calculate_P
     (Cipher  : in FF3_Cipher;
      Round   : in Round_Number;
      W       : in FF3_W;
      Half    : in Digit_Array;
      P       : out Big_Integer;
      Success : out Boolean)
   is
      -- NIST FF3 P calculation with proper byte reversal
      -- P = REVB(AES-ECB(K, REVB(W XOR [i]_4 || NUM_radix(REV(B)))))
      AES_Input : AES_Block;
      Round_Byte : constant Byte := Byte (Round);
      Reversed_Half : constant Digit_Array := Reverse_Digits (Half);
      Half_BigInt : constant Big_Integer := From_Digit_Array (Reversed_Half, Cipher.Radix);
      Half_Bytes : constant Byte_Array := To_Byte_Array (Half_BigInt, 12); -- Pad to 12 bytes
   begin
      -- First 4 bytes: W XOR with round number in the last byte of W
      AES_Input (1 .. 4) := W;
      AES_Input (4) := AES_Input (4) xor Round_Byte;

      -- Last 12 bytes: NUM_radix(REV(B)) padded to 12 bytes
      if Half_Bytes'Length <= 12 then
         -- Pad with zeros at the beginning (big-endian)
         AES_Input (5 .. 16 - Half_Bytes'Length) := [others => 0];
         AES_Input (17 - Half_Bytes'Length .. 16) := Half_Bytes;
      else
         -- Take last 12 bytes if too long
         AES_Input (5 .. 16) := Half_Bytes (Half_Bytes'Last - 11 .. Half_Bytes'Last);
      end if;

      -- Apply FF3 byte reversal convention: REVB before AES
      declare
         Reversed_Input : constant AES_Block := Reverse_Bytes (AES_Input);
         AES_Output : AES_Block;
      begin
         -- Encrypt with AES
         Encrypt_Block (Cipher.AES_Context, Reversed_Input, AES_Output, Success);

         if Success then
            -- Apply FF3 byte reversal convention: REVB after AES
            declare
               Reversed_Output : constant AES_Block := Reverse_Bytes (AES_Output);
            begin
               -- Convert to BigInt
               P := From_Byte_Array (Byte_Array (Reversed_Output));
            end;
         end if;
      end;
   end Calculate_P;

   -- Calculate modulus for FF3 (radix^length)
   function Calculate_Modulus
     (Cipher : FF3_Cipher;
      Length : Text_Length_Type) return Big_Integer
   is
   begin
      return Power (Cipher.Radix, Natural (Length));
   end Calculate_Modulus;

   -- Helper function to convert byte array to big integer
   function From_Byte_Array (Bytes : Byte_Array) return Big_Integer is
      Result : Big_Integer := Zero;
   begin
      for B of Bytes loop
         Result := Multiply_By_Natural (Result, 256);
         Result := Add (Result, From_Natural (Natural (B)));
      end loop;
      return Result;
   end From_Byte_Array;

end FF3_Core;