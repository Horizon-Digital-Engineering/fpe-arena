-- NIST FF3 Test Vector Runner
-- Validates all 15 official NIST test vectors for FF3
-- This is the definitive test that our implementation must pass!

pragma SPARK_Mode (Off); -- Using I/O and file operations

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with FF3_Types; use FF3_Types;
with FF3_Cipher; use FF3_Cipher;

procedure Test_NIST_Vectors is

   -- NIST Test Vector Record
   type NIST_Vector is record
      Sample      : Natural;
      Key_Hex     : Unbounded_String;  -- Variable length key in hex
      Radix       : Radix_Type;
      Plaintext   : Unbounded_String;
      Tweak_Hex   : String (1 .. 16);  -- 64-bit tweak in hex
      Ciphertext  : Unbounded_String;
   end record;

   -- All 15 Official NIST FF3 Test Vectors
   NIST_Vectors : constant array (1 .. 15) of NIST_Vector := [
      -- Vector 1
      (Sample => 1,
       Key_Hex => To_Unbounded_String("EF4359D8D580AA4F7F036D6F04FC6A94"),
       Radix => 10,
       Plaintext => To_Unbounded_String("890121234567890000"),
       Tweak_Hex => "D8E7920AFA330A73",
       Ciphertext => To_Unbounded_String("750918814058654607")),

      -- Vector 2
      (Sample => 2,
       Key_Hex => To_Unbounded_String("EF4359D8D580AA4F7F036D6F04FC6A94"),
       Radix => 10,
       Plaintext => To_Unbounded_String("890121234567890000"),
       Tweak_Hex => "9A768A92F60E12D8",
       Ciphertext => To_Unbounded_String("018989839189395384")),

      -- Vector 3
      (Sample => 3,
       Key_Hex => To_Unbounded_String("EF4359D8D580AA4F7F036D6F04FC6A94"),
       Radix => 10,
       Plaintext => To_Unbounded_String("89012123456789000000789000000"),
       Tweak_Hex => "D8E7920AFA330A73",
       Ciphertext => To_Unbounded_String("48598367162252569629397416226")),

      -- Vector 4
      (Sample => 4,
       Key_Hex => To_Unbounded_String("EF4359D8D580AA4F7F036D6F04FC6A94"),
       Radix => 10,
       Plaintext => To_Unbounded_String("89012123456789000000789000000"),
       Tweak_Hex => "0000000000000000",
       Ciphertext => To_Unbounded_String("34695224821734535122613701434")),

      -- Vector 5
      (Sample => 5,
       Key_Hex => To_Unbounded_String("EF4359D8D580AA4F7F036D6F04FC6A94"),
       Radix => 26,
       Plaintext => To_Unbounded_String("0123456789abcdefghi"),
       Tweak_Hex => "9A768A92F60E12D8",
       Ciphertext => To_Unbounded_String("g2pk40i992fn20cjakb")),

      -- Vector 6
      (Sample => 6,
       Key_Hex => To_Unbounded_String("EF4359D8D580AA4F7F036D6F04FC6A942B7E151628AED2A6"),
       Radix => 10,
       Plaintext => To_Unbounded_String("890121234567890000"),
       Tweak_Hex => "D8E7920AFA330A73",
       Ciphertext => To_Unbounded_String("646965393875028755")),

      -- Vector 7
      (Sample => 7,
       Key_Hex => To_Unbounded_String("EF4359D8D580AA4F7F036D6F04FC6A942B7E151628AED2A6"),
       Radix => 10,
       Plaintext => To_Unbounded_String("890121234567890000"),
       Tweak_Hex => "9A768A92F60E12D8",
       Ciphertext => To_Unbounded_String("961610514491424446")),

      -- Vector 8
      (Sample => 8,
       Key_Hex => To_Unbounded_String("EF4359D8D580AA4F7F036D6F04FC6A942B7E151628AED2A6"),
       Radix => 10,
       Plaintext => To_Unbounded_String("89012123456789000000789000000"),
       Tweak_Hex => "D8E7920AFA330A73",
       Ciphertext => To_Unbounded_String("53048884065350204541786380807")),

      -- Vector 9
      (Sample => 9,
       Key_Hex => To_Unbounded_String("EF4359D8D580AA4F7F036D6F04FC6A942B7E151628AED2A6"),
       Radix => 10,
       Plaintext => To_Unbounded_String("89012123456789000000789000000"),
       Tweak_Hex => "0000000000000000",
       Ciphertext => To_Unbounded_String("98083802678820389295041483512")),

      -- Vector 10
      (Sample => 10,
       Key_Hex => To_Unbounded_String("EF4359D8D580AA4F7F036D6F04FC6A942B7E151628AED2A6"),
       Radix => 26,
       Plaintext => To_Unbounded_String("0123456789abcdefghi"),
       Tweak_Hex => "9A768A92F60E12D8",
       Ciphertext => To_Unbounded_String("i0ihe2jfj7a9opf9p88")),

      -- Vector 11
      (Sample => 11,
       Key_Hex => To_Unbounded_String("EF4359D8D580AA4F7F036D6F04FC6A942B7E151628AED2A6ABF7158809CF4F3C"),
       Radix => 10,
       Plaintext => To_Unbounded_String("890121234567890000"),
       Tweak_Hex => "D8E7920AFA330A73",
       Ciphertext => To_Unbounded_String("922011205562777495")),

      -- Vector 12
      (Sample => 12,
       Key_Hex => To_Unbounded_String("EF4359D8D580AA4F7F036D6F04FC6A942B7E151628AED2A6ABF7158809CF4F3C"),
       Radix => 10,
       Plaintext => To_Unbounded_String("890121234567890000"),
       Tweak_Hex => "9A768A92F60E12D8",
       Ciphertext => To_Unbounded_String("504149865578056140")),

      -- Vector 13
      (Sample => 13,
       Key_Hex => To_Unbounded_String("EF4359D8D580AA4F7F036D6F04FC6A942B7E151628AED2A6ABF7158809CF4F3C"),
       Radix => 10,
       Plaintext => To_Unbounded_String("89012123456789000000789000000"),
       Tweak_Hex => "D8E7920AFA330A73",
       Ciphertext => To_Unbounded_String("04344343235792599165734622699")),

      -- Vector 14
      (Sample => 14,
       Key_Hex => To_Unbounded_String("EF4359D8D580AA4F7F036D6F04FC6A942B7E151628AED2A6ABF7158809CF4F3C"),
       Radix => 10,
       Plaintext => To_Unbounded_String("89012123456789000000789000000"),
       Tweak_Hex => "0000000000000000",
       Ciphertext => To_Unbounded_String("30859239999374053872365555822")),

      -- Vector 15
      (Sample => 15,
       Key_Hex => To_Unbounded_String("EF4359D8D580AA4F7F036D6F04FC6A942B7E151628AED2A6ABF7158809CF4F3C"),
       Radix => 26,
       Plaintext => To_Unbounded_String("0123456789abcdefghi"),
       Tweak_Hex => "9A768A92F60E12D8",
       Ciphertext => To_Unbounded_String("p0b2godfja9bhb7bk38"))
   ];

   -- Hex string to byte array conversion
   function Hex_To_Bytes (Hex_Str : String) return Byte_Array is
      Result : Byte_Array (1 .. Hex_Str'Length / 2);
      Hex_Map : constant array (Character range '0' .. 'F') of Natural :=
         ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7,
          '8' => 8, '9' => 9, 'A' => 10, 'B' => 11, 'C' => 12, 'D' => 13, 'E' => 14, 'F' => 15,
          others => 0];
   begin
      for I in Result'Range loop
         declare
            Idx : constant Natural := 2 * I - 1;
            High : constant Character := Hex_Str (Idx);
            Low : constant Character := Hex_Str (Idx + 1);
         begin
            Result (I) := Byte (Hex_Map (High) * 16 + Hex_Map (Low));
         end;
      end loop;
      return Result;
   end Hex_To_Bytes;

   -- String to digit array conversion for given radix
   function String_To_Digits (Text : String; Radix : Radix_Type) return Digit_Array is
      Result : Digit_Array (1 .. Text'Length);
   begin
      for I in Text'Range loop
         declare
            C : constant Character := Text (I);
         begin
            if C in '0' .. '9' then
               Result (I - Text'First + 1) := Character'Pos (C) - Character'Pos ('0');
            elsif C in 'a' .. 'z' then
               Result (I - Text'First + 1) := Character'Pos (C) - Character'Pos ('a') + 10;
            elsif C in 'A' .. 'Z' then
               Result (I - Text'First + 1) := Character'Pos (C) - Character'Pos ('A') + 10;
            else
               raise Constraint_Error with "Invalid character in string: " & C;
            end if;
         end;
      end loop;
      return Result;
   end String_To_Digits;

   -- Digit array to string conversion for given radix
   function Digits_To_String (Digit_Values : Digit_Array; Radix : Radix_Type) return String is
      Result : String (1 .. Digit_Values'Length);
   begin
      for I in Digit_Values'Range loop
         declare
            D : constant Digit_Value := Digit_Values (I);
         begin
            if D < 10 then
               Result (I - Digit_Values'First + 1) := Character'Val (Character'Pos ('0') + D);
            elsif D < 36 then
               Result (I - Digit_Values'First + 1) := Character'Val (Character'Pos ('a') + D - 10);
            elsif D < 62 then
               Result (I - Digit_Values'First + 1) := Character'Val (Character'Pos ('A') + D - 36);
            else
               raise Constraint_Error with "Invalid digit value: " & D'Image;
            end if;
         end;
      end loop;
      return Result;
   end Digits_To_String;

   -- Test single NIST vector
   procedure Test_NIST_Vector (Vector : NIST_Vector; Success : out Boolean) is
      Key : constant Byte_Array := Hex_To_Bytes (To_String (Vector.Key_Hex));
      Tweak : constant FF3_Tweak := Hex_To_Bytes (Vector.Tweak_Hex);

      Plaintext_Str : constant String := To_String (Vector.Plaintext);
      Expected_Ciphertext_Str : constant String := To_String (Vector.Ciphertext);

      Plaintext_Digits : constant Digit_Array := String_To_Digits (Plaintext_Str, Vector.Radix);
      Expected_Ciphertext_Digits : constant Digit_Array := String_To_Digits (Expected_Ciphertext_Str, Vector.Radix);

      Cipher : FF3_Cipher.FF3_Cipher;
      Init_Status : Init_Result;

      Actual_Ciphertext : Digit_Array (Plaintext_Digits'Range);
      Decrypted_Plaintext : Digit_Array (Plaintext_Digits'Range);

      Encrypt_Success, Decrypt_Success : Boolean;
   begin
      Success := False;

      Put ("Vector" & Vector.Sample'Image & " ");

      -- Initialize cipher
      Initialize_Cipher (Cipher, Vector.Radix, Key, Tweak, Init_Status);
      if Init_Status /= FF3_Cipher.Success then
         Put_Line ("❌ FAIL (init)");
         return;
      end if;

      -- Encrypt
      Encrypt_Digits (Cipher, Plaintext_Digits, Actual_Ciphertext, Encrypt_Success);
      if not Encrypt_Success then
         Put_Line ("❌ FAIL (encrypt)");
         return;
      end if;

      -- Check encryption result
      if Actual_Ciphertext /= Expected_Ciphertext_Digits then
         Put_Line ("❌ FAIL (wrong ciphertext)");
         Put_Line ("  Expected: " & Expected_Ciphertext_Str);
         Put_Line ("  Got:      " & Digits_To_String (Actual_Ciphertext, Vector.Radix));
         return;
      end if;

      -- Decrypt
      Decrypt_Digits (Cipher, Actual_Ciphertext, Decrypted_Plaintext, Decrypt_Success);
      if not Decrypt_Success then
         Put_Line ("❌ FAIL (decrypt)");
         return;
      end if;

      -- Check decryption result (round trip)
      if Decrypted_Plaintext /= Plaintext_Digits then
         Put_Line ("❌ FAIL (round trip)");
         Put_Line ("  Original: " & Plaintext_Str);
         Put_Line ("  Decrypted:" & Digits_To_String (Decrypted_Plaintext, Vector.Radix));
         return;
      end if;

      Put_Line ("✅ PASS");
      Success := True;
   end Test_NIST_Vector;

   -- Main test execution
   Total_Tests : constant Natural := NIST_Vectors'Length;
   Passed_Tests : Natural := 0;
   Vector_Success : Boolean;

begin
   Put_Line ("🧪 NIST FF3 Official Test Vector Validation");
   Put_Line ("============================================");
   Put_Line ("Testing" & Total_Tests'Image & " official NIST SP 800-38G vectors");
   New_Line;

   -- Run all test vectors
   for I in NIST_Vectors'Range loop
      Test_NIST_Vector (NIST_Vectors (I), Vector_Success);
      if Vector_Success then
         Passed_Tests := Passed_Tests + 1;
      end if;
   end loop;

   New_Line;
   Put_Line ("📊 TEST RESULTS:");
   Put_Line ("================");
   Put ("Passed: "); Put (Passed_Tests); New_Line;
   Put ("Failed: "); Put (Total_Tests - Passed_Tests); New_Line;
   Put ("Total:  "); Put (Total_Tests); New_Line;
   New_Line;

   if Passed_Tests = Total_Tests then
      Put_Line ("🏆 SUCCESS! ALL NIST VECTORS PASS!");
      Put_Line ("✅ FF3 implementation is NIST-compliant!");
      Put_Line ("🚀 Ready for production use!");
   else
      Put_Line ("❌ FAILURE: Some NIST vectors failed!");
      Put_Line ("🔧 Implementation needs fixes before deployment!");
   end if;

exception
   when E : others =>
      Put_Line ("💥 CRITICAL ERROR during NIST testing!");
      Put_Line ("Exception: " & Ada.Exceptions.Exception_Information (E));
end Test_NIST_Vectors;