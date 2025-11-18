-- FF3 CLI - Command-line tool for FF3 encryption/decryption

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with GNAT.Command_Line; use GNAT.Command_Line;
with FF3_Types; use FF3_Types;
with FF3_API; use FF3_API;
with FF3_Alphabets; use FF3_Alphabets;

procedure FF3_CLI is

   Encrypt_Text : Unbounded_String;
   Decrypt_Text : Unbounded_String;
   Key_Hex : Unbounded_String;
   Tweak_Hex : Unbounded_String;
   Alphabet_Type : Unbounded_String := To_Unbounded_String ("digits");
   Custom_Charset : Unbounded_String;
   Show_Help : Boolean := False;

   procedure Show_Usage is
   begin
      Put_Line ("FF3 CLI - Format Preserving Encryption");
      New_Line;
      Put_Line ("Usage: ff3-cli [OPTIONS]");
      New_Line;
      Put_Line ("Options:");
      Put_Line ("  -e, --encrypt TEXT      Encrypt the given text");
      Put_Line ("  -d, --decrypt TEXT      Decrypt the given text");
      Put_Line ("  -k, --key HEX           AES key in hex format (32/48/64 hex chars)");
      Put_Line ("  -t, --tweak HEX         Tweak in hex format (16 hex chars)");
      Put_Line ("  -a, --alphabet TYPE     Alphabet type:");
      Put_Line ("                            digits (default)");
      Put_Line ("                            hex-lower");
      Put_Line ("                            hex-upper");
      Put_Line ("                            base36-lower");
      Put_Line ("                            base36-upper");
      Put_Line ("                            base62");
      Put_Line ("  -c, --custom CHARSET    Custom alphabet charset");
      Put_Line ("  -h, --help              Show this help message");
      New_Line;
      Put_Line ("Examples:");
      Put_Line ("  ff3-cli -e ""1234567890"" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73");
      Put_Line ("  ff3-cli -d ""7501889140"" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73");
      New_Line;
   end Show_Usage;

   function Hex_To_Byte (C : Character) return Byte is
   begin
      case C is
         when '0' .. '9' => return Character'Pos (C) - Character'Pos ('0');
         when 'a' .. 'f' => return Character'Pos (C) - Character'Pos ('a') + 10;
         when 'A' .. 'F' => return Character'Pos (C) - Character'Pos ('A') + 10;
         when others => raise Constraint_Error with "Invalid hex character";
      end case;
   end Hex_To_Byte;

   function Parse_Hex (Hex_Str : String) return Byte_Array is
      Result : Byte_Array (1 .. Hex_Str'Length / 2);
      Idx : Natural := Result'First;
   begin
      if Hex_Str'Length mod 2 /= 0 then
         raise Constraint_Error with "Hex string must have even length";
      end if;

      for I in Hex_Str'First .. Hex_Str'Last - 1 loop
         if (I - Hex_Str'First) mod 2 = 0 then
            Result (Idx) := Hex_To_Byte (Hex_Str (I)) * 16 +
                           Hex_To_Byte (Hex_Str (I + 1));
            Idx := Idx + 1;
         end if;
      end loop;

      return Result;
   end Parse_Hex;

   function Parse_Tweak (Hex_Str : String) return FF3_Tweak is
      Bytes : constant Byte_Array := Parse_Hex (Hex_Str);
   begin
      if Bytes'Length /= 8 then
         raise Constraint_Error with "Tweak must be 8 bytes (16 hex chars)";
      end if;
      return FF3_Tweak (Bytes);
   end Parse_Tweak;

   procedure Create_Cipher
     (Cipher : out FF3_String_Cipher;
      Key : Byte_Array;
      Tweak : FF3_Tweak;
      Alphabet_Name : String;
      Custom : String;
      Result : out String_Result) is
   begin
      if Custom'Length > 0 then
         declare
            Spec : constant Alphabet_Spec := FF3_Alphabets.Create_Alphabet_Spec (Custom);
         begin
            Create_Custom_Cipher (Cipher, Key, Tweak, Spec, Result);
         end;
      elsif Alphabet_Name = "digits" then
         Create_Digits_Cipher (Cipher, Key, Tweak, Result);
      elsif Alphabet_Name = "hex-lower" then
         Create_Hex_Lower_Cipher (Cipher, Key, Tweak, Result);
      elsif Alphabet_Name = "hex-upper" then
         Create_Hex_Upper_Cipher (Cipher, Key, Tweak, Result);
      elsif Alphabet_Name = "base36-lower" then
         Create_Base36_Lower_Cipher (Cipher, Key, Tweak, Result);
      elsif Alphabet_Name = "base36-upper" then
         Create_Base36_Upper_Cipher (Cipher, Key, Tweak, Result);
      elsif Alphabet_Name = "base62" then
         Create_Base62_Cipher (Cipher, Key, Tweak, Result);
      else
         raise Constraint_Error with "Unknown alphabet type: " & Alphabet_Name;
      end if;
   end Create_Cipher;

begin
   -- Parse command line
   begin
      loop
         case Getopt ("e: d: k: t: a: c: h -encrypt: -decrypt: -key: -tweak: -alphabet: -custom: -help") is
            when 'e' =>
               Encrypt_Text := To_Unbounded_String (Parameter);
            when 'd' =>
               Decrypt_Text := To_Unbounded_String (Parameter);
            when 'k' =>
               Key_Hex := To_Unbounded_String (Parameter);
            when 't' =>
               Tweak_Hex := To_Unbounded_String (Parameter);
            when 'a' =>
               Alphabet_Type := To_Unbounded_String (Parameter);
            when 'c' =>
               Custom_Charset := To_Unbounded_String (Parameter);
            when 'h' =>
               Show_Help := True;
               exit;
            when ASCII.NUL =>
               exit;
            when others =>
               raise Program_Error;
         end case;
      end loop;

   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Put_Line (Standard_Error, "Error: Invalid command line option");
         Show_Usage;
         Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      when GNAT.Command_Line.Invalid_Parameter =>
         Put_Line (Standard_Error, "Error: Invalid parameter");
         Show_Usage;
         Set_Exit_Status (Ada.Command_Line.Failure);
         return;
   end;

   if Show_Help then
      Show_Usage;
      return;
   end if;

   -- Validate required arguments
   if Length (Encrypt_Text) = 0 and Length (Decrypt_Text) = 0 then
      Put_Line (Standard_Error, "Error: Either --encrypt or --decrypt must be specified");
      Show_Usage;
      Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   if Length (Key_Hex) = 0 or Length (Tweak_Hex) = 0 then
      Put_Line (Standard_Error, "Error: Key (-k) and tweak (-t) are required");
      Show_Usage;
      Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   declare
      Key : constant Byte_Array := Parse_Hex (To_String (Key_Hex));
      Tweak : constant FF3_Tweak := Parse_Tweak (To_String (Tweak_Hex));
      Cipher : FF3_String_Cipher;
      Result : String_Result;
   begin
      -- Validate key length
      if Key'Length /= 16 and Key'Length /= 24 and Key'Length /= 32 then
         Put_Line (Standard_Error, "Error: Key must be 16, 24, or 32 bytes (32, 48, or 64 hex chars)");
         Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;

      -- Create cipher
      Create_Cipher (Cipher, Key, Tweak, To_String (Alphabet_Type),
                    To_String (Custom_Charset), Result);

      if Result /= FF3_API.Success then
         Put_Line (Standard_Error, "Error: Failed to create cipher");
         Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;

      -- Encrypt or decrypt
      if Length (Encrypt_Text) > 0 then
         declare
            Plaintext : constant String := To_String (Encrypt_Text);
            Ciphertext : String (Plaintext'Range);
         begin
            Encrypt_String (Cipher, Plaintext, Ciphertext, Result);
            if Result /= FF3_API.Success then
               Put_Line (Standard_Error, "Error: Encryption failed");
               Set_Exit_Status (Ada.Command_Line.Failure);
               return;
            end if;
            Put_Line (Ciphertext);
         end;
      elsif Length (Decrypt_Text) > 0 then
         declare
            Ciphertext : constant String := To_String (Decrypt_Text);
            Plaintext : String (Ciphertext'Range);
         begin
            Decrypt_String (Cipher, Ciphertext, Plaintext, Result);
            if Result /= FF3_API.Success then
               Put_Line (Standard_Error, "Error: Decryption failed");
               Set_Exit_Status (Ada.Command_Line.Failure);
               return;
            end if;
            Put_Line (Plaintext);
         end;
      end if;

   exception
      when E : others =>
         Put_Line (Standard_Error, "Error: " & Ada.Exceptions.Exception_Message (E));
         Set_Exit_Status (Ada.Command_Line.Failure);
   end;

end FF3_CLI;
