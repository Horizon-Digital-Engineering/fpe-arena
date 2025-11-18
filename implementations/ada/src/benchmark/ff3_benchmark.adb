-- FF3 Performance Benchmark Tool with Full CLI Support and JSON Output
-- Measures FF3 encryption/decryption performance

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with GNAT.OS_Lib;
with GNATCOLL.JSON; use GNATCOLL.JSON;

with FF3_Types; use FF3_Types;
with FF3_API; use FF3_API;

procedure FF3_Benchmark is

   -- Configuration
   type Config_Type is record
      Alphabet      : Unbounded_String := To_Unbounded_String ("digits");
      Radix         : Natural := 10;
      Lengths       : String (1 .. 100) := [others => ' '];
      Cases         : String (1 .. 100) := [others => ' '];
      Iterations    : Natural := 100_000;
      Warmup        : Natural := 10_000;
      Key_Hex       : String (1 .. 32) := "EF4359D8D580AA4F7F036D6F04FC6A94";
      Tweak_Hex     : String (1 .. 16) := "D8E7920AFA330A73";
      Seed          : Natural := 42;
      Quick         : Boolean := False;
      Verbose       : Boolean := False;
      JSON_Out      : Unbounded_String := Null_Unbounded_String;
   end record;

   Config : Config_Type;

   -- Array types for lengths and cases
   type Length_Array is array (Positive range <>) of Natural;
   type Case_Array is array (Positive range <>) of Unbounded_String;

   -- Benchmark result type
   type Benchmark_Result is record
      Name : Unbounded_String;
      Test_Case : Unbounded_String;
      Length : Natural;
      Iterations : Natural;
      Elapsed_NS : Long_Long_Integer;
      NS_Per_Op : Long_Long_Integer;
      Ops_Per_Sec : Natural;
   end record;

   type Benchmark_Array is array (Positive range <>) of Benchmark_Result;

   procedure Print_Help is
   begin
      Put_Line ("FF3 Performance Benchmark Tool");
      Put_Line ("");
      Put_Line ("Usage: ff3_benchmark [OPTIONS]");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  --config <file>      Load configuration from JSON file");
      Put_Line ("  --alphabet <name>    Alphabet to use (default: digits)");
      Put_Line ("  --iterations <n>     Number of iterations (default: 100000)");
      Put_Line ("  --warmup <n>         Warmup iterations (default: 10000)");
      Put_Line ("  --key <hex>          Hex-encoded key (default: EF4359D8...)");
      Put_Line ("  --tweak <hex>        Hex-encoded tweak (default: D8E7920A...)");
      Put_Line ("  --seed <n>           Random seed (default: 42)");
      Put_Line ("  --quick              Reduce iterations by 10x");
      Put_Line ("  --json-out <file>    Write JSON output to file");
      Put_Line ("  --verbose            Show progress messages");
      Put_Line ("  -h, --help           Show this help message");
      Put_Line ("");
      Put_Line ("⚠️  FF3 was withdrawn by NIST due to security vulnerabilities.");
      Put_Line ("   This tool is for educational and research purposes only.");
   end Print_Help;

   procedure Load_Config_File (Filename : String) is
      Input_File : File_Type;
      File_Contents : Unbounded_String;
      Line : String (1 .. 4096);
      Last : Natural;
      JSON_Val : JSON_Value;
   begin
      -- Read entire file
      Open (Input_File, In_File, Filename);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Line, Last);
         Append (File_Contents, Line (1 .. Last));
      end loop;
      Close (Input_File);

      -- Parse JSON
      JSON_Val := Read (To_String (File_Contents));

      -- Extract scalar fields
      if Has_Field (JSON_Val, "alphabet") then
         declare
            Alphabet_Str : constant String := GNATCOLL.JSON.Get (JSON_Val, "alphabet");
         begin
            Config.Alphabet := To_Unbounded_String (Alphabet_Str);
         end;
      end if;

      if Has_Field (JSON_Val, "radix") then
         Config.Radix := GNATCOLL.JSON.Get (JSON_Val, "radix");
      end if;

      if Has_Field (JSON_Val, "iterations") then
         Config.Iterations := GNATCOLL.JSON.Get (JSON_Val, "iterations");
      end if;

      if Has_Field (JSON_Val, "warmup") then
         Config.Warmup := GNATCOLL.JSON.Get (JSON_Val, "warmup");
      end if;

      if Has_Field (JSON_Val, "key") then
         declare
            Key_Str : constant String := GNATCOLL.JSON.Get (JSON_Val, "key");
         begin
            Config.Key_Hex (1 .. Key_Str'Length) := Key_Str;
         end;
      end if;

      if Has_Field (JSON_Val, "tweak") then
         declare
            Tweak_Str : constant String := GNATCOLL.JSON.Get (JSON_Val, "tweak");
         begin
            Config.Tweak_Hex (1 .. Tweak_Str'Length) := Tweak_Str;
         end;
      end if;

      if Has_Field (JSON_Val, "seed") then
         Config.Seed := GNATCOLL.JSON.Get (JSON_Val, "seed");
      end if;

      -- Extract lengths array
      if Has_Field (JSON_Val, "lengths") then
         declare
            Lengths_Arr : constant JSON_Array := GNATCOLL.JSON.Get (JSON_Val, "lengths");
            Lengths_Str : Unbounded_String;
         begin
            for I in 1 .. Length (Lengths_Arr) loop
               if I > 1 then
                  Ada.Strings.Unbounded.Append (Lengths_Str, ",");
               end if;
               Ada.Strings.Unbounded.Append (Lengths_Str, Integer'Image (GNATCOLL.JSON.Get (GNATCOLL.JSON.Get (Lengths_Arr, I))));
            end loop;
            Config.Lengths (1 .. Ada.Strings.Unbounded.Length (Lengths_Str)) := To_String (Lengths_Str);
         end;
      end if;

      -- Extract cases array
      if Has_Field (JSON_Val, "cases") then
         declare
            Cases_Arr : constant JSON_Array := GNATCOLL.JSON.Get (JSON_Val, "cases");
            Cases_Str : Unbounded_String;
         begin
            for I in 1 .. Length (Cases_Arr) loop
               if I > 1 then
                  Ada.Strings.Unbounded.Append (Cases_Str, ",");
               end if;
               declare
                  Case_Val : constant JSON_Value := GNATCOLL.JSON.Get (Cases_Arr, I);
                  Case_Str : constant String := GNATCOLL.JSON.Get (Case_Val);
               begin
                  Ada.Strings.Unbounded.Append (Cases_Str, Case_Str);
               end;
            end loop;
            Config.Cases (1 .. Ada.Strings.Unbounded.Length (Cases_Str)) := To_String (Cases_Str);
         end;
      end if;

   exception
      when E : others =>
         Put_Line ("ERR: Failed to load config file: " & Filename);
         GNAT.OS_Lib.OS_Exit (2);
   end Load_Config_File;

   -- Parse lengths from Config.Lengths string (e.g., " 9, 12")
   function Parse_Lengths return Length_Array is
      Lengths_Str : constant String := Trim (Config.Lengths, Ada.Strings.Both);
      Result : Length_Array (1 .. 10);  -- Max 10 lengths
      Count : Natural := 0;
      Start_Pos : Natural := 1;
   begin
      if Lengths_Str'Length = 0 or else (for all C of Lengths_Str => C = ' ') then
         return (1 => 9, 2 => 12, 3 => 16);
      end if;

      for I in Lengths_Str'Range loop
         if Lengths_Str (I) = ',' or I = Lengths_Str'Last then
            declare
               End_Pos : constant Natural := (if Lengths_Str (I) = ',' then I - 1 else I);
               Num_Str : constant String := Trim (Lengths_Str (Start_Pos .. End_Pos), Ada.Strings.Both);
            begin
               if Num_Str'Length > 0 then
                  Count := Count + 1;
                  Result (Count) := Natural'Value (Num_Str);
               end if;
            end;
            Start_Pos := I + 1;
         end if;
      end loop;

      return Result (1 .. Count);
   end Parse_Lengths;

   -- Parse cases from Config.Cases string (e.g., "enc,dec")
   function Parse_Cases return Case_Array is
      Cases_Str : constant String := Trim (Config.Cases, Ada.Strings.Both);
      Result : Case_Array (1 .. 5);  -- Max 5 cases
      Count : Natural := 0;
      Start_Pos : Natural := 1;
   begin
      if Cases_Str'Length = 0 or else (for all C of Cases_Str => C = ' ') then
         return (1 => To_Unbounded_String ("enc"), 2 => To_Unbounded_String ("dec"));
      end if;

      for I in Cases_Str'Range loop
         if Cases_Str (I) = ',' or I = Cases_Str'Last then
            declare
               End_Pos : constant Natural := (if Cases_Str (I) = ',' then I - 1 else I);
               Case_Str : constant String := Trim (Cases_Str (Start_Pos .. End_Pos), Ada.Strings.Both);
            begin
               if Case_Str'Length > 0 then
                  Count := Count + 1;
                  Result (Count) := To_Unbounded_String (Case_Str);
               end if;
            end;
            Start_Pos := I + 1;
         end if;
      end loop;

      return Result (1 .. Count);
   end Parse_Cases;

   procedure Parse_Args is
      I : Natural := 1;
      Config_File : Unbounded_String := Null_Unbounded_String;
   begin
      while I <= Argument_Count loop
         declare
            Arg : constant String := Argument (I);
         begin
            if Arg = "-h" or Arg = "--help" then
               Print_Help;
               GNAT.OS_Lib.OS_Exit (0);
            elsif Arg = "--quick" then
               Config.Quick := True;
            elsif Arg = "--verbose" then
               Config.Verbose := True;
            elsif Arg = "--config" and I < Argument_Count then
               I := I + 1;
               Config_File := To_Unbounded_String (Argument (I));
            elsif Arg = "--alphabet" and I < Argument_Count then
               I := I + 1;
               Config.Alphabet := To_Unbounded_String (Argument (I));
            elsif Arg = "--iterations" and I < Argument_Count then
               I := I + 1;
               Config.Iterations := Natural'Value (Argument (I));
            elsif Arg = "--warmup" and I < Argument_Count then
               I := I + 1;
               Config.Warmup := Natural'Value (Argument (I));
            elsif Arg = "--key" and I < Argument_Count then
               I := I + 1;
               Config.Key_Hex := Argument (I);
            elsif Arg = "--tweak" and I < Argument_Count then
               I := I + 1;
               Config.Tweak_Hex := Argument (I);
            elsif Arg = "--json-out" and I < Argument_Count then
               I := I + 1;
               Config.JSON_Out := To_Unbounded_String (Argument (I));
            end if;
         end;
         I := I + 1;
      end loop;

      -- Load config file if specified
      if Config_File /= Null_Unbounded_String then
         Load_Config_File (To_String (Config_File));
      end if;

      -- Apply quick mode
      if Config.Quick then
         Config.Iterations := Config.Iterations / 10;
         Config.Warmup := Config.Warmup / 10;
      end if;
   end Parse_Args;

   -- Generate test input of given length using digits alphabet
   function Generate_Input (Length : Natural; Index : Natural) return String is
      Result : String (1 .. Length);
      Digit_Val : Natural;
   begin
      for I in Result'Range loop
         -- Simple pseudo-random pattern based on index and position
         Digit_Val := ((Index * 7) + (I * 13)) mod 10;
         Result (I) := Character'Val (Character'Pos ('0') + Digit_Val);
      end loop;
      return Result;
   end Generate_Input;

   -- Run a single benchmark case
   function Run_Single_Benchmark (
      Cipher : FF3_String_Cipher;
      Test_Case : String;
      Length : Natural) return Benchmark_Result
   is
      -- Generate 64 test inputs as ring buffer
      type Input_Array is array (1 .. 64) of access String;
      Inputs : Input_Array;
      Precomputed_CTs : Input_Array;

      Start_Time, End_Time : Time;
      Elapsed_Time : Time_Span;
      Result : Benchmark_Result;
   begin
      -- Generate inputs
      for I in Inputs'Range loop
         Inputs (I) := new String'(Generate_Input (Length, I));
      end loop;

      -- Pre-compute ciphertexts for decryption benchmarks
      if Test_Case = "dec" then
         for I in Inputs'Range loop
            Precomputed_CTs (I) := new String (1 .. Length);
            declare
               Cipher_Result : String_Result;
            begin
               Encrypt_String (Cipher, Inputs (I).all, Precomputed_CTs (I).all, Cipher_Result);
            end;
         end loop;
      end if;

      -- Warmup phase
      for I in 1 .. Config.Warmup loop
         declare
            Input_Idx : constant Positive := ((I - 1) mod 64) + 1;
            Dummy_Output : String (1 .. Length);
            Dummy_Result : String_Result;
         begin
            if Test_Case = "enc" then
               Encrypt_String (Cipher, Inputs (Input_Idx).all, Dummy_Output, Dummy_Result);
            elsif Test_Case = "dec" then
               Decrypt_String (Cipher, Precomputed_CTs (Input_Idx).all, Dummy_Output, Dummy_Result);
            end if;
         end;
      end loop;

      -- Measured benchmark
      Start_Time := Clock;
      for I in 1 .. Config.Iterations loop
         declare
            Input_Idx : constant Positive := ((I - 1) mod 64) + 1;
            Output : String (1 .. Length);
            Op_Result : String_Result;
         begin
            if Test_Case = "enc" then
               Encrypt_String (Cipher, Inputs (Input_Idx).all, Output, Op_Result);
            elsif Test_Case = "dec" then
               Decrypt_String (Cipher, Precomputed_CTs (Input_Idx).all, Output, Op_Result);
            end if;
         end;
      end loop;
      End_Time := Clock;

      Elapsed_Time := End_Time - Start_Time;
      Result.Name := To_Unbounded_String (Test_Case & "_len" & Natural'Image (Length) & "_radix10");
      Result.Test_Case := To_Unbounded_String (Test_Case);
      Result.Length := Length;
      Result.Iterations := Config.Iterations;
      Result.Elapsed_NS := Long_Long_Integer (Float (To_Duration (Elapsed_Time)) * 1_000_000_000.0);
      Result.NS_Per_Op := Result.Elapsed_NS / Long_Long_Integer (Config.Iterations);
      Result.Ops_Per_Sec := Natural (Float (Config.Iterations) / Float (To_Duration (Elapsed_Time)));

      -- Free allocated inputs
      for I in Inputs'Range loop
         if Inputs (I) /= null then
            declare
               Temp : access String := Inputs (I);
            begin
               Inputs (I) := null;
            end;
         end if;
      end loop;

      if Test_Case = "dec" then
         for I in Precomputed_CTs'Range loop
            if Precomputed_CTs (I) /= null then
               declare
                  Temp : access String := Precomputed_CTs (I);
               begin
                  Precomputed_CTs (I) := null;
               end;
            end if;
         end loop;
      end if;

      return Result;
   end Run_Single_Benchmark;

   function Hex_To_Byte (C : Character) return Byte is
   begin
      case C is
         when '0' .. '9' => return Character'Pos (C) - Character'Pos ('0');
         when 'a' .. 'f' => return Character'Pos (C) - Character'Pos ('a') + 10;
         when 'A' .. 'F' => return Character'Pos (C) - Character'Pos ('A') + 10;
         when others => return 0;
      end case;
   end Hex_To_Byte;

   function Parse_Hex_Key (Hex : String) return Byte_Array is
      Result : Byte_Array (1 .. Hex'Length / 2);
   begin
      for I in Result'Range loop
         declare
            Idx : constant Positive := (I - 1) * 2 + Hex'First;
         begin
            Result (I) := Hex_To_Byte (Hex (Idx)) * 16 + Hex_To_Byte (Hex (Idx + 1));
         end;
      end loop;
      return Result;
   end Parse_Hex_Key;

   function Parse_Hex_Tweak (Hex : String) return FF3_Tweak is
      Result : FF3_Tweak;
   begin
      for I in Result'Range loop
         declare
            Idx : constant Positive := (I - 1) * 2 + Hex'First;
         begin
            Result (I) := Hex_To_Byte (Hex (Idx)) * 16 + Hex_To_Byte (Hex (Idx + 1));
         end;
      end loop;
      return Result;
   end Parse_Hex_Tweak;

   function To_JSON_Timestamp return String is
      use Ada.Calendar;
      use Ada.Calendar.Formatting;
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      return Image (Now, Time_Zone => 0) & "Z";
   end To_JSON_Timestamp;

   procedure Output_JSON (Benchmarks : Benchmark_Array) is
      Output_File : File_Type;
      Use_File : constant Boolean := Config.JSON_Out /= Null_Unbounded_String;

      -- Helper procedure to output to either stdout or file
      procedure Write_Line (S : String) is
      begin
         if Use_File then
            Put_Line (Output_File, S);
         else
            Put_Line (S);
         end if;
      end Write_Line;

   begin
      if Use_File then
         Create (Output_File, Out_File, To_String (Config.JSON_Out));
      end if;

      -- Build JSON output
      Write_Line ("{");
      Write_Line ("  ""metadata"": {");
      Write_Line ("    ""version"": ""1.0"",");
      Write_Line ("    ""timestamp"": """ & To_JSON_Timestamp & """,");
      Write_Line ("    ""language"": ""ada/spark"",");
      Write_Line ("    ""runtime"": ""GNAT"",");
      Write_Line ("    ""platform"": {");
      Write_Line ("      ""os"": ""linux"",");
      Write_Line ("      ""arch"": ""x86_64"",");
      Write_Line ("      ""cpu"": ""unknown"",");
      Write_Line ("      ""cores"": 1");
      Write_Line ("    }");
      Write_Line ("  },");
      Write_Line ("  ""configuration"": {");
      Write_Line ("    ""seed"":" & Natural'Image (Config.Seed) & ",");
      Write_Line ("    ""warmup_iterations"":" & Natural'Image (Config.Warmup));
      Write_Line ("  },");
      Write_Line ("  ""benchmarks"": [");

      -- Output all benchmarks
      for I in Benchmarks'Range loop
         declare
            B : Benchmark_Result renames Benchmarks (I);
            Name_Trimmed : constant String := Trim (To_String (B.Name), Ada.Strings.Both);
            Case_Trimmed : constant String := Trim (To_String (B.Test_Case), Ada.Strings.Both);
         begin
            Write_Line ("    {");
            Write_Line ("      ""name"": """ & Name_Trimmed & """,");
            Write_Line ("      ""test_case"": """ & Case_Trimmed & """,");
            Write_Line ("      ""parameters"": {");
            Write_Line ("        ""alphabet"": """ & To_String (Config.Alphabet) & """,");
            Write_Line ("        ""radix"":" & Natural'Image (Config.Radix) & ",");
            Write_Line ("        ""length"":" & Natural'Image (B.Length) & ",");
            Write_Line ("        ""key_bits"": 128,");
            Write_Line ("        ""key_fingerprint"": """ & Config.Key_Hex (1 .. 8) & """,");
            Write_Line ("        ""tweak"": """ & Config.Tweak_Hex & """");
            Write_Line ("      },");
            Write_Line ("      ""iterations"":" & Natural'Image (B.Iterations) & ",");
            Write_Line ("      ""elapsed_ns"":" & Long_Long_Integer'Image (B.Elapsed_NS) & ",");
            Write_Line ("      ""ns_per_op"":" & Long_Long_Integer'Image (B.NS_Per_Op) & ",");
            Write_Line ("      ""ops_per_sec"":" & Natural'Image (B.Ops_Per_Sec) & ",");
            Write_Line ("      ""checksum"": ""00000000""");
            if I < Benchmarks'Last then
               Write_Line ("    },");
            else
               Write_Line ("    }");
            end if;
         end;
      end loop;

      Write_Line ("  ],");
      Write_Line ("  ""summary"": {");
      Write_Line ("    ""total_tests"":" & Natural'Image (Benchmarks'Length) & ",");

      declare
         Total_NS : Long_Long_Integer := 0;
      begin
         for B of Benchmarks loop
            Total_NS := Total_NS + B.Elapsed_NS;
         end loop;
         Write_Line ("    ""total_duration_sec"":" & Float'Image (Float (Total_NS) / 1_000_000_000.0) & ",");
      end;

      Write_Line ("    ""checksum"": ""00000000""");
      Write_Line ("  }");
      Write_Line ("}");

      if Use_File then
         Close (Output_File);
      end if;
   end Output_JSON;

begin
   -- Parse command-line arguments
   Parse_Args;

   -- Verbose human output to stderr
   if Config.Verbose then
      Put_Line (Standard_Error, "🚀 FF3 Performance Benchmark Tool v1.0.0");
      Put_Line (Standard_Error, "========================================");
      Put_Line (Standard_Error, "");
      Put_Line (Standard_Error, "⚠️  FF3 was WITHDRAWN by NIST due to security vulnerabilities.");
      Put_Line (Standard_Error, "    This benchmark is for EDUCATIONAL and RESEARCH purposes only.");
      Put_Line (Standard_Error, "");
      Put_Line (Standard_Error, "🔧 Configuration:");
      Put_Line (Standard_Error, "   Algorithm: FF3 (" & To_String (Config.Alphabet) & " radix)");
      Put_Line (Standard_Error, "   Key: " & Natural'Image (Config.Key_Hex'Length * 4) & "-bit AES");
      Put_Line (Standard_Error, "   Platform: Ada/SPARK");
      Put_Line (Standard_Error, "   Operations per test:" & Natural'Image (Config.Iterations));
      Put_Line (Standard_Error, "");
   end if;

   -- Run benchmarks
   declare
      Cipher : FF3_String_Cipher;
      Result : String_Result;
      Key : constant Byte_Array := Parse_Hex_Key (Config.Key_Hex);
      Tweak : constant FF3_Tweak := Parse_Hex_Tweak (Config.Tweak_Hex);

      Lengths : constant Length_Array := Parse_Lengths;
      Cases : constant Case_Array := Parse_Cases;

      -- Allocate max possible benchmarks
      All_Benchmarks : Benchmark_Array (1 .. 50);
      Benchmark_Count : Natural := 0;
   begin
      -- Initialize cipher
      if To_String (Config.Alphabet) = "digits" then
         Create_Digits_Cipher (Cipher, Key, Tweak, Result);
      else
         Put_Line (Standard_Error, "⚠️  Only digits alphabet implemented for now");
         return;
      end if;

      if Result /= FF3_API.Success then
         Put_Line (Standard_Error, "❌ Cipher initialization failed");
         return;
      end if;

      -- Loop through all length/case combinations
      for Length of Lengths loop
         for Test_Case of Cases loop
            if Config.Verbose then
               Put_Line (Standard_Error, "📊 Running " & To_String (Test_Case) & "_len" & Natural'Image (Length) & "...");
            end if;

            declare
               Bench_Result : constant Benchmark_Result := Run_Single_Benchmark (Cipher, To_String (Test_Case), Length);
            begin
               Benchmark_Count := Benchmark_Count + 1;
               All_Benchmarks (Benchmark_Count) := Bench_Result;

               if Config.Verbose then
                  Put_Line (Standard_Error, "   " & Long_Long_Integer'Image (Bench_Result.NS_Per_Op) &
                            " ns/op (" & Natural'Image (Bench_Result.Ops_Per_Sec) & " ops/sec)");
               end if;
            end;
         end loop;
      end loop;

      -- Output JSON results
      Output_JSON (All_Benchmarks (1 .. Benchmark_Count));
   end;

end FF3_Benchmark;
