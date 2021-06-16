with Ada.Unchecked_Conversion;

package body RSmalltalk.Memory is

   function Word2Int is new Ada.Unchecked_Conversion(Source => T_Word,
                                                     Target => T_Int);

   --------------------
   -- integerValueOf --
   --------------------

   function isIntegerValue (value : T_Word) return Boolean is
   begin
      return (Word2Int(value) >= T_CompactSmallInteger'First)
        and (Word2Int(value) <= T_CompactSmallInteger'Last);
   end isIntegerValue;

   function isIntegerObject(ptr: T_Pointer) return Boolean
   is
   begin
      return (ptr mod 2) = 1;
   end isIntegerObject;

   function integerObjectOf (value : T_Int) return T_Pointer is
   begin
      return T_Pointer(value * 2 + 1);
   end integerObjectOf;


   function integerValueOf (ptr : T_Pointer) return T_Int is
   begin
      return T_CompactSmallInteger(Word2Int(ptr) / 2);
   end integerValueOf;

end RSmalltalk.Memory;
