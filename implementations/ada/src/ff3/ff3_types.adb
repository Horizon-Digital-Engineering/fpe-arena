-- FF3 Types Implementation

package body FF3_Types is

   -- Implementation of validation predicates

   function Valid_Digit_Array_For_Radix
     (Values : Digit_Array; Radix : Radix_Type) return Boolean
   is
   begin
      for Digit of Values loop
         if not Valid_Digit_For_Radix (Digit, Radix) then
            return False;
         end if;
      end loop;
      return True;
   end Valid_Digit_Array_For_Radix;

end FF3_Types;