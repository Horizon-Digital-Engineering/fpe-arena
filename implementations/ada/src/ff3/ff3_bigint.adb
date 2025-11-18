-- FF3 BigInteger Implementation using Ada Standard Library
-- Wrapper around Ada.Numerics.Big_Numbers.Big_Integers

with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;

package body FF3_BigInt is

   -- Constructor functions
   function Zero return Big_Integer is
   begin
      return To_Big_Integer (0);
   end Zero;

   function From_Natural (Value : Natural) return Big_Integer is
   begin
      return To_Big_Integer (Value);
   end From_Natural;

   function From_Digit_Array (Values : Digit_Array; Radix : Radix_Type) return Big_Integer is
      Result : Big_Integer := Zero;
      Radix_Big : Big_Integer := To_Big_Integer (Integer (Radix));
   begin
      for Digit of Values loop
         Result := Result * Radix_Big + To_Big_Integer (Integer (Digit));
      end loop;
      return Result;
   end From_Digit_Array;

   -- Conversion functions
   function To_Digit_Array (Value : Big_Integer; Radix : Radix_Type; Length : Text_Length_Type) return Digit_Array is
      Result : Digit_Array (1 .. Natural (Length)) := (others => 0);
      Temp : Big_Integer := Value;
      Radix_Big : Big_Integer := To_Big_Integer (Integer (Radix));
      Index : Natural := Result'Last;
   begin
      -- Convert from least significant to most significant
      while Temp > Zero and Index >= Result'First loop
         declare
            Remainder : Big_Integer := Temp mod Radix_Big;
         begin
            Result (Index) := Natural (To_Integer (Remainder));
            Temp := Temp / Radix_Big;
            Index := Index - 1;
         end;
      end loop;
      return Result;
   end To_Digit_Array;

   function To_Byte_Array (Value : Big_Integer; Length : Positive) return Byte_Array is
      Result : Byte_Array (1 .. Length) := (others => 0);
      Temp : Big_Integer := Value;
      Base_256 : Big_Integer := To_Big_Integer (256);
      Index : Natural := Length;
   begin
      -- Convert to bytes (big-endian)
      while Temp > Zero and Index >= Result'First loop
         declare
            Remainder : Big_Integer := Temp mod Base_256;
         begin
            Result (Index) := Byte (To_Integer (Remainder));
            Temp := Temp / Base_256;
            Index := Index - 1;
         end;
      end loop;
      return Result;
   end To_Byte_Array;

   -- Arithmetic operations
   function Add (Left, Right : Big_Integer) return Big_Integer is
   begin
      return Left + Right;
   end Add;

   function Subtract (Left, Right : Big_Integer) return Big_Integer is
   begin
      return Left - Right;
   end Subtract;

   function Mod_Add (Left, Right, Modulus : Big_Integer) return Big_Integer is
   begin
      return (Left + Right) mod Modulus;
   end Mod_Add;

   function Mod_Subtract (Left, Right, Modulus : Big_Integer) return Big_Integer is
   begin
      if Left >= Right then
         return (Left - Right) mod Modulus;
      else
         -- Handle negative result by adding modulus
         return (Modulus - ((Right - Left) mod Modulus)) mod Modulus;
      end if;
   end Mod_Subtract;

   function Modulo (Value, Modulus : Big_Integer) return Big_Integer is
   begin
      return Value mod Modulus;
   end Modulo;

   function Multiply_By_Radix (Value : Big_Integer; Radix : Radix_Type) return Big_Integer is
   begin
      return Value * To_Big_Integer (Integer (Radix));
   end Multiply_By_Radix;

   function Power (Base : Radix_Type; Exponent : Natural) return Big_Integer is
      Result : Big_Integer := To_Big_Integer (1);
      Base_Big : Big_Integer := To_Big_Integer (Integer (Base));
   begin
      for I in 1 .. Exponent loop
         Result := Result * Base_Big;
      end loop;
      return Result;
   end Power;

   -- Helper procedures
   procedure Divide_By_Radix
     (Value : in Big_Integer;
      Radix : in Radix_Type;
      Quotient : out Big_Integer;
      Remainder : out Natural)
   is
      Radix_Big : Big_Integer := To_Big_Integer (Integer (Radix));
   begin
      Quotient := Value / Radix_Big;
      Remainder := Natural (To_Integer (Value mod Radix_Big));
   end Divide_By_Radix;

   function Multiply_By_Natural (Value : Big_Integer; Multiplier : Natural) return Big_Integer is
   begin
      return Value * To_Big_Integer (Multiplier);
   end Multiply_By_Natural;

   -- Comparison functions
   function Is_Zero (Value : Big_Integer) return Boolean is
   begin
      return Value = Zero;
   end Is_Zero;

   function Is_Less_Than (Left, Right : Big_Integer) return Boolean is
   begin
      return Left < Right;
   end Is_Less_Than;

   function Equals (Left, Right : Big_Integer) return Boolean is
   begin
      return Left = Right;
   end Equals;

end FF3_BigInt;