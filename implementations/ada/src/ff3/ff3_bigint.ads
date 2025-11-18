-- FF3 BigInteger Package using Ada Standard Library
-- Wrapper around Ada.Numerics.Big_Numbers.Big_Integers

with FF3_Types; use FF3_Types;
with Ada.Numerics.Big_Numbers.Big_Integers;

package FF3_BigInt is

   -- Use Ada standard Big_Integer type
   subtype Big_Integer is Ada.Numerics.Big_Numbers.Big_Integers.Big_Integer;

   -- Constructor functions
   function Zero return Big_Integer;
   function From_Natural (Value : Natural) return Big_Integer;
   function From_Digit_Array (Values : Digit_Array; Radix : Radix_Type) return Big_Integer;

   -- Conversion functions
   function To_Digit_Array (Value : Big_Integer; Radix : Radix_Type; Length : Text_Length_Type) return Digit_Array;
   function To_Byte_Array (Value : Big_Integer; Length : Positive) return Byte_Array;

   -- Arithmetic operations
   function Add (Left, Right : Big_Integer) return Big_Integer;
   function Subtract (Left, Right : Big_Integer) return Big_Integer;
   function Mod_Add (Left, Right, Modulus : Big_Integer) return Big_Integer;
   function Mod_Subtract (Left, Right, Modulus : Big_Integer) return Big_Integer;
   function Modulo (Value, Modulus : Big_Integer) return Big_Integer;
   function Multiply_By_Radix (Value : Big_Integer; Radix : Radix_Type) return Big_Integer;
   function Power (Base : Radix_Type; Exponent : Natural) return Big_Integer;

   -- Helper procedures
   procedure Divide_By_Radix
     (Value : in Big_Integer;
      Radix : in Radix_Type;
      Quotient : out Big_Integer;
      Remainder : out Natural);

   function Multiply_By_Natural (Value : Big_Integer; Multiplier : Natural) return Big_Integer;

   -- Comparison functions
   function Is_Zero (Value : Big_Integer) return Boolean;
   function Is_Less_Than (Left, Right : Big_Integer) return Boolean;
   function Equals (Left, Right : Big_Integer) return Boolean;

end FF3_BigInt;