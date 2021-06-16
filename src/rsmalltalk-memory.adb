with Ada.Unchecked_Conversion;

package body RSmalltalk.Memory is

   function Ptr2Smi is new Ada.Unchecked_Conversion(Source => T_Pointer,
                                                    Target => T_SmallInt);

   function Smi2Ptr is new Ada.Unchecked_Conversion(Source => T_SmallInt,
                                                    Target => T_Pointer);

   function Word2Int is new Ada.Unchecked_Conversion(Source => T_Word,
                                                     Target => T_Int);

   --------------------
   -- integerValueOf --
   --------------------

   function integerValueOf (ptr : T_Pointer) return T_Int is
   begin
      return Ptr2Smi( ptr ).value;
   end integerValueOf;

   ---------------------
   -- integerObjectOf --
   ---------------------

   function integerObjectOf (value : T_Int) return T_Pointer is
   begin
      return T_Pointer(value * 2 + 1);
   end integerObjectOf;

   --------------------
   -- isIntegerValue --
   --------------------

   function isIntegerValue (value : T_Word) return Boolean is
   begin
      return (Word2Int(value) >= T_CompactSmallInteger'First)
        and (Word2Int(value) <= T_CompactSmallInteger'Last);
   end isIntegerValue;


end RSmalltalk.Memory;
