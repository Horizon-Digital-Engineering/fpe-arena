-- FF3 Alphabets - Implementation
-- Character set conversion functions

package body FF3_Alphabets is

   function Create_Alphabet_Spec (Characters : String) return Alphabet_Spec is
      Spec : Alphabet_Spec;
   begin
      Spec.Length := Radix_Type (Characters'Length);

      -- Copy characters to fixed-size array
      Spec.Characters (1 .. Characters'Length) := Characters;

      -- Fill remaining with null characters
      if Characters'Length < Max_Radix then
         Spec.Characters (Characters'Length + 1 .. Max_Radix) := [others => Character'Val (0)];
      end if;

      return Spec;
   end Create_Alphabet_Spec;

   function Digits_Spec return Alphabet_Spec is
   begin
      return Create_Alphabet_Spec (DIGITS_ALPHABET);
   end Digits_Spec;

   function Hex_Lower_Spec return Alphabet_Spec is
   begin
      return Create_Alphabet_Spec (HEX_LOWER_ALPHABET);
   end Hex_Lower_Spec;

   function Hex_Upper_Spec return Alphabet_Spec is
   begin
      return Create_Alphabet_Spec (HEX_UPPER_ALPHABET);
   end Hex_Upper_Spec;

   function Base36_Lower_Spec return Alphabet_Spec is
   begin
      return Create_Alphabet_Spec (BASE36_LOWER_ALPHABET);
   end Base36_Lower_Spec;

   function Base36_Upper_Spec return Alphabet_Spec is
   begin
      return Create_Alphabet_Spec (BASE36_UPPER_ALPHABET);
   end Base36_Upper_Spec;

   function Base62_Spec return Alphabet_Spec is
   begin
      return Create_Alphabet_Spec (BASE62_ALPHABET);
   end Base62_Spec;

   function Char_To_Digit (Char : Character; Spec : Alphabet_Spec) return Digit_Value is
   begin
      for I in 1 .. Natural (Spec.Length) loop
         if Spec.Characters (I) = Char then
            return Digit_Value (I - 1);
         end if;
      end loop;
      return 0; -- Invalid character returns 0
   end Char_To_Digit;

   function Digit_To_Char (Digit : Digit_Value; Spec : Alphabet_Spec) return Character is
   begin
      return Spec.Characters (Natural (Digit) + 1);
   end Digit_To_Char;

   function Valid_Character (Char : Character; Spec : Alphabet_Spec) return Boolean is
   begin
      for I in 1 .. Natural (Spec.Length) loop
         if Spec.Characters (I) = Char then
            return True;
         end if;
      end loop;
      return False;
   end Valid_Character;

   function Valid_String (Text : String; Spec : Alphabet_Spec) return Boolean is
   begin
      for I in Text'Range loop
         if not Valid_Character (Text (I), Spec) then
            return False;
         end if;
      end loop;
      return True;
   end Valid_String;

   procedure String_To_Digit_Array
     (Text        : in String;
      Spec        : in Alphabet_Spec;
      Digit_Array : out FF3_Types.Digit_Array;
      Success     : out Boolean) is
   begin
      Success := True;

      if not Valid_String (Text, Spec) then
         Success := False;
         return;
      end if;

      for I in Text'Range loop
         declare
            Digit_Index : constant Natural := I - Text'First + Digit_Array'First;
         begin
            Digit_Array (Digit_Index) := Char_To_Digit (Text (I), Spec);
         end;
      end loop;
   end String_To_Digit_Array;

   procedure Digit_Array_To_String
     (Digit_Array : in FF3_Types.Digit_Array;
      Spec        : in Alphabet_Spec;
      Text        : out String;
      Success     : out Boolean) is
   begin
      Success := True;

      for I in Digit_Array'Range loop
         declare
            Text_Index : constant Natural := I - Digit_Array'First + Text'First;
         begin
            if Digit_Array (I) >= Natural (Spec.Length) then
               Success := False;
               return;
            end if;
            Text (Text_Index) := Digit_To_Char (Digit_Array (I), Spec);
         end;
      end loop;
   end Digit_Array_To_String;

end FF3_Alphabets;