-- FF3 Alphabets - Character Set Definitions
-- This package defines standard alphabets for Format Preserving Encryption

with FF3_Types; use FF3_Types;

package FF3_Alphabets is
   pragma Pure;

   -- Standard alphabet constants (order is part of the spec!)
   DIGITS_ALPHABET : constant String := "0123456789";                                                 -- radix 10
   HEX_LOWER_ALPHABET : constant String := "0123456789abcdef";                                        -- radix 16 lowercase
   HEX_UPPER_ALPHABET : constant String := "0123456789ABCDEF";                                        -- radix 16 uppercase
   BASE36_LOWER_ALPHABET : constant String := "0123456789abcdefghijklmnopqrstuvwxyz";                 -- radix 36 lowercase
   BASE36_UPPER_ALPHABET : constant String := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";                 -- radix 36 uppercase
   BASE62_ALPHABET : constant String := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"; -- radix 62

   -- Alphabet specification type
   type Alphabet_Spec is record
      Characters : String (1 .. Max_Radix);
      Length     : Radix_Type;
   end record;

   -- Create alphabet spec from string
   function Create_Alphabet_Spec (Characters : String) return Alphabet_Spec;

   -- Built-in alphabet specifications
   function Digits_Spec return Alphabet_Spec;
   function Hex_Lower_Spec return Alphabet_Spec;
   function Hex_Upper_Spec return Alphabet_Spec;
   function Base36_Lower_Spec return Alphabet_Spec;
   function Base36_Upper_Spec return Alphabet_Spec;
   function Base62_Spec return Alphabet_Spec;

   -- Character lookup functions
   function Char_To_Digit (Char : Character; Spec : Alphabet_Spec) return Digit_Value;
   function Digit_To_Char (Digit : Digit_Value; Spec : Alphabet_Spec) return Character;

   -- Validation functions
   function Valid_Character (Char : Character; Spec : Alphabet_Spec) return Boolean;
   function Valid_String (Text : String; Spec : Alphabet_Spec) return Boolean;

   -- Convert string to digit array
   procedure String_To_Digit_Array
     (Text        : in String;
      Spec        : in Alphabet_Spec;
      Digit_Array : out FF3_Types.Digit_Array;
      Success     : out Boolean);

   -- Convert digit array to string
   procedure Digit_Array_To_String
     (Digit_Array : in FF3_Types.Digit_Array;
      Spec        : in Alphabet_Spec;
      Text        : out String;
      Success     : out Boolean);

end FF3_Alphabets;
