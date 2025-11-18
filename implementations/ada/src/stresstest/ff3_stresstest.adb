with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;          use Ada.Text_IO;
with FF3_API;              use FF3_API;
with FF3_Alphabets;
with FF3_Types;            use FF3_Types;

procedure FF3_StressTest is
   package Float_Random renames Ada.Numerics.Float_Random;
   Generator : Float_Random.Generator;

   type Options is record
      Iterations : Positive := 1000;
      Min_Length : Positive := 6;
      Max_Length : Positive := 20;
      Quick      : Boolean  := False;
      Seed       : Integer  := 0;
      Seed_Set   : Boolean  := False;
      Help       : Boolean  := False;
   end record;

   package Name_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Unbounded_String);
   use Name_Vectors;

   function Default_Alphabet_Names return Vector is
      Result : Vector;
   begin
      Result.Append (To_Unbounded_String ("digits"));
      Result.Append (To_Unbounded_String ("hex-lower"));
      Result.Append (To_Unbounded_String ("base36-lower"));
      Result.Append (To_Unbounded_String ("base62"));
      return Result;
   end Default_Alphabet_Names;

   type Options_With_List is record
      Data      : Options;
      Alphabets : Vector := Default_Alphabet_Names;
   end record;

   function To_Lower (Value : String) return String is
      use Ada.Characters.Handling;
      Result : String := Value;
   begin
      for I in Result'Range loop
         Result (I) := To_Lower (Result (I));
      end loop;
      return Result;
   end To_Lower;

   procedure Print_Usage is
   begin
      Put_Line ("FF3 Stress Test Tool");
      New_Line;
      Put_Line ("Usage: ff3_stresstest [OPTIONS] [ITERATIONS]");
      New_Line;
      Put_Line ("Options:");
      Put_Line ("  --alphabets LIST      Comma-separated list (default: digits,hex-lower,base36-lower,base62)");
      Put_Line ("  --min-length N        Minimum plaintext length (default: 6)");
      Put_Line ("  --max-length N        Maximum plaintext length (default: 20)");
      Put_Line ("  --quick               Run 100 iterations (fast test)");
      Put_Line ("  --seed N              Random seed for reproducibility");
   end Print_Usage;

   function Parse_Options return Options_With_List is
      use Ada.Command_Line;
      Parsed : Options_With_List;
      Index  : Positive := 1;
   begin
      while Index <= Argument_Count loop
         declare
            Arg   : constant String := Argument (Index);
            Lower : constant String := To_Lower (Arg);
         begin
            if Lower = "--help" or else Lower = "-h" then
               Parsed.Data.Help := True;
               return Parsed;
            elsif Lower = "--alphabets" then
               if Index = Argument_Count then
                  raise Constraint_Error with "--alphabets requires a value";
               end if;
               Parsed.Alphabets.Clear;
               declare
                  Value : constant String := Argument (Index + 1);
                  Start : Positive := Value'First;
               begin
                  for Pos in Value'Range loop
                     if Value (Pos) = ',' then
                        if Pos > Start then
                           Parsed.Alphabets.Append
                             (To_Unbounded_String (To_Lower (Value (Start .. Pos - 1))));
                        end if;
                        Start := Pos + 1;
                     elsif Pos = Value'Last then
                        Parsed.Alphabets.Append
                          (To_Unbounded_String (To_Lower (Value (Start .. Pos))));
                     end if;
                  end loop;
               end;
               if Parsed.Alphabets.Is_Empty then
                  raise Constraint_Error with "--alphabets list cannot be empty";
               end if;
               Index := Index + 1;
            elsif Lower = "--min-length" then
               if Index = Argument_Count then
                  raise Constraint_Error with "--min-length requires a value";
               end if;
               Parsed.Data.Min_Length := Positive'Value (Argument (Index + 1));
               Index := Index + 1;
            elsif Lower = "--max-length" then
               if Index = Argument_Count then
                  raise Constraint_Error with "--max-length requires a value";
               end if;
               Parsed.Data.Max_Length := Positive'Value (Argument (Index + 1));
               Index := Index + 1;
            elsif Lower = "--quick" then
               Parsed.Data.Quick := True;
            elsif Lower = "--seed" then
               if Index = Argument_Count then
                  raise Constraint_Error with "--seed requires a value";
               end if;
               Parsed.Data.Seed     := Integer'Value (Argument (Index + 1));
               Parsed.Data.Seed_Set := True;
               Index := Index + 1;
            elsif Arg (1) = '-' then
               raise Constraint_Error with "Unknown option: " & Arg;
            else
               Parsed.Data.Iterations := Positive'Value (Arg);
            end if;
         end;
         Index := Index + 1;
      end loop;
      return Parsed;
   end Parse_Options;

   procedure Validate (Opts : Options_With_List) is
   begin
      if Opts.Data.Iterations = 0 then
         raise Constraint_Error with "iterations must be greater than 0";
      elsif Opts.Data.Min_Length < 2 then
         raise Constraint_Error with "--min-length must be at least 2";
      elsif Opts.Data.Max_Length < Opts.Data.Min_Length then
         raise Constraint_Error with "--max-length must be greater than or equal to --min-length";
      elsif Opts.Alphabets.Is_Empty then
         raise Constraint_Error with "--alphabets list cannot be empty";
      end if;
   end Validate;

   function Join (List : Vector) return String is
      Result     : Unbounded_String := Null_Unbounded_String;
      Position   : Cursor := List.First;
      First_Node : constant Cursor := Position;
   begin
      while Has_Element (Position) loop
         if Position /= First_Node then
            Append (Result, ", ");
         end if;
         Append (Result, Element (Position));
         Next (Position);
      end loop;
      return To_String (Result);
   end Join;

   function Lookup_Alphabet (Name : String) return FF3_Alphabets.Alphabet_Spec is
      Lower : constant String := To_Lower (Name);
   begin
      if Lower = "digits" then
         return FF3_Alphabets.Digits_Spec;
      elsif Lower = "hex-lower" then
         return FF3_Alphabets.Hex_Lower_Spec;
      elsif Lower = "hex-upper" then
         return FF3_Alphabets.Hex_Upper_Spec;
      elsif Lower = "base36-lower" then
         return FF3_Alphabets.Base36_Lower_Spec;
      elsif Lower = "base36-upper" then
         return FF3_Alphabets.Base36_Upper_Spec;
      elsif Lower = "base62" then
         return FF3_Alphabets.Base62_Spec;
      else
         raise Constraint_Error with "Unknown alphabet: " & Name;
      end if;
   end Lookup_Alphabet;

   function Bytes_To_Hex (Value : Byte_Array) return String is
      Hex_Digits : constant String := "0123456789abcdef";
      Result     : String (1 .. Value'Length * 2);
      Pos        : Positive := 1;
   begin
      for Byte of Value loop
         declare
            Indices : constant Natural := Natural (Byte);
         begin
            Result (Pos)     := Hex_Digits (Indices / 16 + 1);
            Result (Pos + 1) := Hex_Digits (Indices mod 16 + 1);
            Pos := Pos + 2;
         end;
      end loop;
      return Result;
   end Bytes_To_Hex;

   function Random_Byte return Byte is
   begin
      return Byte (Integer (Float_Random.Random (Generator) * 256.0));
   end Random_Byte;

   function Random_Bytes (Count : Positive) return Byte_Array is
      Result : Byte_Array (1 .. Count);
   begin
      for Index in Result'Range loop
         Result (Index) := Random_Byte;
      end loop;
      return Result;
   end Random_Bytes;

   function Random_Tweak return FF3_Tweak is
      Result : FF3_Tweak;
   begin
      for Index in Result'Range loop
         Result (Index) := Random_Byte;
      end loop;
      return Result;
   end Random_Tweak;

   Double_Quote : constant String := """";

   function Random_Length (Min_Length, Max_Length : Positive) return Positive is
      Length_Range : constant Positive := Max_Length - Min_Length + 1;
   begin
      return Min_Length + Positive (Integer (Float_Random.Random (Generator) * Float (Length_Range)));
   end Random_Length;

   function Random_String (Alphabet : String; Length : Positive) return String is
      Result : String (1 .. Length);
   begin
      for Index in Result'Range loop
         declare
            Position : constant Positive :=
              Alphabet'First + Positive (Integer (Float_Random.Random (Generator) * Float (Alphabet'Length)));
         begin
            Result (Index) := Alphabet (Position);
         end;
      end loop;
      return Result;
   end Random_String;

   procedure Print_Failure
     (Key        : Byte_Array;
      Tweak      : Byte_Array;
      Plaintext  : String;
      Ciphertext : String;
      Decrypted  : String;
      Detail     : String) is
   begin
      Put_Line ("  Round-trip failed:");
      Put_Line ("    Key: " & Bytes_To_Hex (Key));
      Put_Line ("    Tweak: " & Bytes_To_Hex (Tweak));
      Put_Line ("    Plaintext: " & Double_Quote & Plaintext & Double_Quote);
      if Ciphertext'Length > 0 then
         Put_Line ("    Ciphertext: " & Double_Quote & Ciphertext & Double_Quote);
      end if;
      if Decrypted'Length > 0 then
         Put_Line ("    Decrypted: " & Double_Quote & Decrypted & Double_Quote);
      end if;
      if Detail'Length > 0 then
         Put_Line ("    Detail: " & Detail);
      end if;
   end Print_Failure;

   procedure Create_Cipher
     (Name   : String;
      Key    : Byte_Array;
      Tweak  : FF3_Tweak;
      Cipher : out FF3_String_Cipher;
      Result : out String_Result) is
   begin
      if Name = "digits" then
         Create_Digits_Cipher (Cipher, Key, Tweak, Result);
      elsif Name = "hex-lower" then
         Create_Hex_Lower_Cipher (Cipher, Key, Tweak, Result);
      elsif Name = "hex-upper" then
         Create_Hex_Upper_Cipher (Cipher, Key, Tweak, Result);
      elsif Name = "base36-lower" then
         Create_Base36_Lower_Cipher (Cipher, Key, Tweak, Result);
      elsif Name = "base36-upper" then
         Create_Base36_Upper_Cipher (Cipher, Key, Tweak, Result);
      elsif Name = "base62" then
         Create_Base62_Cipher (Cipher, Key, Tweak, Result);
      else
         declare
            Spec : constant FF3_Alphabets.Alphabet_Spec := Lookup_Alphabet (Name);
         begin
            Create_Custom_Cipher (Cipher, Key, Tweak, Spec, Result);
         end;
      end if;
   end Create_Cipher;

   procedure Stress_Alphabet
     (Name : String;
      Spec : FF3_Alphabets.Alphabet_Spec;
      Opts : Options_With_List;
      Tests : out Natural;
      Failures : out Natural) is
      Characters : constant String := Spec.Characters (1 .. Natural (Spec.Length));
      Interval   : constant Natural := Natural'Max (1, Opts.Data.Iterations / 10);
      Passed     : Natural := 0;
      Failed     : Natural := 0;
   begin
      Put_Line ("Testing " & Name & "...");
      Put_Line ("  Alphabet: " & Characters & " (radix " & Integer'Image (Characters'Length) & ")");

      for I in 1 .. Opts.Data.Iterations loop
         declare
            Key   : constant Byte_Array := Random_Bytes (16);
            Tweak : constant FF3_Tweak := Random_Tweak;
            Len   : constant Natural := Random_Length (Opts.Data.Min_Length, Opts.Data.Max_Length);
            Plain : constant String := Random_String (Characters, Len);
            Cipher : FF3_String_Cipher;
            Result : String_Result;
            Ciphertext, Decrypted : String (Plain'Range);
            Encrypt_Result, Decrypt_Result : String_Result;
         begin
            Create_Cipher (Name, Key, Tweak, Cipher, Result);
            if Result /= Success then
               Failed := Failed + 1;
               Print_Failure (Key, Tweak, Plain, "", "", "cipher initialization failed");
            else
               Encrypt_String (Cipher, Plain, Ciphertext, Encrypt_Result);
               if Encrypt_Result /= Success then
                  Failed := Failed + 1;
                  Print_Failure (Key, Tweak, Plain, "", "", "encryption error");
               else
                  Decrypt_String (Cipher, Ciphertext, Decrypted, Decrypt_Result);
                  if Decrypt_Result = Success and then Decrypted = Plain then
                     Passed := Passed + 1;
                  else
                     Failed := Failed + 1;
                     Print_Failure (Key, Tweak, Plain, Ciphertext, Decrypted, "round-trip mismatch");
                  end if;
               end if;
            end if;

            if (I mod Interval = 0) or else (I = Opts.Data.Iterations) then
               declare
                  Percent : constant Natural := (I * 100) / Opts.Data.Iterations;
               begin
                  Put_Line ("  Progress: " & Integer'Image (I) & "/" & Integer'Image (Opts.Data.Iterations)
                            & " (" & Integer'Image (Percent) & "%)");
               end;
            end if;
         end;
      end loop;

      Put_Line ("  Passed: " & Integer'Image (Passed) & "/" & Integer'Image (Opts.Data.Iterations));
      Put_Line ("  Failed: " & Integer'Image (Failed) & "/" & Integer'Image (Opts.Data.Iterations));
      New_Line;

      Tests    := Opts.Data.Iterations;
      Failures := Failed;
   end Stress_Alphabet;

begin
   declare
      Opts : Options_With_List := Parse_Options;
   begin
      if Opts.Data.Help then
         Print_Usage;
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
         return;
      end if;

      if Opts.Data.Quick then
         Opts.Data.Iterations := 100;
      end if;

      Validate (Opts);

      if Opts.Data.Seed_Set then
         Float_Random.Reset (Generator, Opts.Data.Seed);
      else
         Float_Random.Reset (Generator);
      end if;

      Put_Line ("FF3 Stress Test v1.0");
      Put_Line ("====================");
      New_Line;
      Put_Line ("Warning: FF3 was withdrawn by NIST; run for education and research only.");
      New_Line;
      Put_Line ("Test configuration");
      Put_Line ("  Iterations per alphabet: " & Integer'Image (Opts.Data.Iterations));
      Put_Line ("  Random key/tweak generation: enabled");
      Put_Line ("  String length range: " & Integer'Image (Opts.Data.Min_Length) & "-"
                & Integer'Image (Opts.Data.Max_Length) & " characters");
      Put_Line ("  Alphabets: " & Join (Opts.Alphabets));
      New_Line;

      declare
         Start_Time     : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Total_Tests    : Natural := 0;
         Total_Failures : Natural := 0;
         Cursor         : Name_Vectors.Cursor := Opts.Alphabets.First;
      begin
         while Name_Vectors.Has_Element (Cursor) loop
            declare
               Name : constant String := To_String (Name_Vectors.Element (Cursor));
               Spec : constant FF3_Alphabets.Alphabet_Spec := Lookup_Alphabet (Name);
               Tests, Failures : Natural;
            begin
               Stress_Alphabet (Name, Spec, Opts, Tests, Failures);
               Total_Tests    := Total_Tests + Tests;
               Total_Failures := Total_Failures + Failures;
            end;
            Name_Vectors.Next (Cursor);
         end loop;

         declare
            Finish       : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            Elapsed      : constant Duration :=
              Ada.Calendar."-" (Finish, Start_Time);
            Milliseconds : constant Long_Integer :=
              Long_Integer (Elapsed * 1_000.0);
         begin
            Put_Line ("Summary");
            Put_Line ("  Total tests: " & Integer'Image (Total_Tests));
            Put_Line ("  Failures: " & Integer'Image (Total_Failures));
            Put_Line ("  Duration: " & Long_Integer'Image (Milliseconds) & " ms");
            if Milliseconds > 0 then
               declare
                  Throughput : constant Float :=
                    Float (Total_Tests) * 1000.0 / Float (Milliseconds);
               begin
                  Put_Line ("  Throughput: " & Float'Image (Throughput) & " tests/sec");
               end;
            end if;
            if Total_Failures = 0 then
               Put_Line ("  Result: all stress tests passed");
               Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
            else
               Put_Line ("  Result: failures detected");
               Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            end if;
         end;
      end;
   end;
exception
   when Constraint_Error =>
      Put_Line ("Error: invalid argument");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   when E : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end FF3_StressTest;
