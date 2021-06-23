with Ada.Unchecked_Conversion;

package body RSmalltalk.Memory is

   type T_BytesInWord is record
      ah : T_Byte;
      al : T_Byte;
   end record;
   for T_BytesInWord'Size use T_Word'Size;

   function Word2Int is new Ada.Unchecked_Conversion(Source => T_Word,
                                                     Target => T_Int);

   function Int2Word is new Ada.Unchecked_Conversion(Source => T_Int,
                                                     Target => T_Word);

   function W2NO is new Ada.Unchecked_Conversion(Source => T_Word,
                                                        Target => T_NumericObject);

   function NO2W is new Ada.Unchecked_Conversion(Source => T_NumericObject,
                                                        Target => T_Word);

   function isSmallIntValue (value : T_Int) return Boolean is
   begin
      return (value >= T_SmallInt'First)
        and (value <= T_SmallInt'Last);
   end isSmallIntValue;
   pragma Inline(isSmallIntValue);

   function isSmallWordValue (value : T_Word) return Boolean is
   begin
      return (value >= T_SmallWord'First)
        and (value <= T_SmallWord'Last);
   end isSmallWordValue;
   pragma Inline(isSmallWordValue);

   function isIntegerObject(so: T_NumericObject) return Boolean
   is
   begin
      return so.int;
   end isIntegerObject;
   pragma Inline(isIntegerObject);

   function isIntegerObject(value: T_Word) return Boolean
   is
   begin
      return (value and 16#1#) = 16#1#;
   end isIntegerObject;
   pragma Inline(isIntegerObject);

   function integerObjectOf (value : T_Int) return T_IntObject
   is
      so : T_IntObject := (int => true, val=> T_SmallInt(value));
   begin
      return so;
   end integerObjectOf;
   pragma Inline(integerObjectOf);

   function wordObjectOf (value : T_Word) return T_IntObject
   is
      so : T_IntObject := (int => true,
                           val=> T_SmallInt(Word2Int(value)));
   begin
      return so;
   end wordObjectOf;
   pragma Inline(wordObjectOf);

   function integerValueOf (so : T_NumericObject) return T_Int is
   begin
      return T_Int(so.val);
   end integerValueOf;
   pragma Inline(integerValueOf);

   function wordValueOf (so : T_NumericObject) return T_Word is
   begin
      return T_Word(Int2Word(so.val));
   end wordValueOf;
   pragma Inline(wordValueOf);

   function rawValueOf (so : T_NumericObject) return T_Word is

   begin
      if so.int then
         return Int2Word(so.val * 2) or 16#1#;
      else
         return T_Word(so.addr * 2);
      end if;
   end rawValueOf;
   pragma Inline(rawValueOf);

   function addressOf (so : T_NumericObject) return T_Word is
   begin
      return T_Word(so.addr);
   end addressOf;
   pragma Inline(addressOf);

   function asIntObject (value : T_Word) return T_IntObject is
      so : T_NumericObject := W2NO(value);
   begin
      return so;
   end asIntObject;
   pragma Inline(asIntObject);

   function asPointer (value : T_Word) return T_Pointer is
      so : T_NumericObject := W2NO(value);
   begin
      return so;
   end asPointer;
   pragma Inline(asPointer);
   ------------------------

   function get
     (rwm     : in T_Memory;
      segment : in T_SegmentIndex;
      index   : in T_Word) return T_Word
   is
      x : T_Word;
      for x'Address use rwm.segment(segment)(index)'Address;
   begin
      return x;
   end get;

   function get
     (rwm       : in T_Memory;
      segment   : in T_SegmentIndex;
      index     : in T_Word;
      byteIndex : in T_Byte) return T_Byte
   is
      x : T_BytesInWord;
      offs : T_Word := (index + T_Word(byteIndex / 2));
      for x'Address use rwm.segment(segment)(offs)'Address;
   begin
      case byteIndex mod 2 is
         when 0 => return x.al;
         when 1 => return x.ah;
         when others => return 0; -- never happens
      end case;
   end get;

   procedure put_internal
     (rwm     : in out T_Memory;
      segment : in     T_SegmentIndex;
      index   : in     T_Word;
      value   : in     T_Word)
   is
      x : T_Word;
      for x'Address use rwm.segment(segment)(index)'Address;
   begin
      x := value;
   end put_internal;
   pragma Inline_Always(put_internal);

   procedure put_internal
     (rwm       : in out T_Memory;
      segment   : in     T_SegmentIndex;
      index     : in     T_Word;
      byteIndex : in     T_Byte;
      value     : in     T_Byte)
   is
      x : T_BytesInWord;
      offs : T_Word := (index + T_Word(byteIndex / 2));
      for x'Address use rwm.segment(segment)(offs)'Address;
   begin
      case byteIndex mod 2 is
         when 0 => x.al := value;
         when 1 => x.ah := value;
         when others => null; -- never happens
      end case;
   end put_internal;
   pragma Inline_Always(put_internal);


   procedure put
     (rwm     : in out T_Memory;
      segment : in     T_SegmentIndex;
      index   : in     T_Word;
      value   : in     T_Word)
   is
   begin
      put_internal(rwm, segment, index, value);
   end put;
   pragma Inline(put);

   procedure put
     (rwm       : in out T_Memory;
      segment   : in     T_SegmentIndex;
      index     : in     T_Word;
      byteIndex : in     T_Byte;
      value     : in     T_Byte)
   is
   begin
      put_internal(rwm, segment, index, byteIndex, value);
   end put;
   pragma Inline(put);

   function put
     (rwm     : in out T_Memory;
      segment : in     T_SegmentIndex;
      index   : in     T_Word;
      value   : in     T_Word) return T_Word
   is
   begin
      put_internal(rwm, segment, index, value);
      return value;
   end put;

   function put
     (rwm       : in out T_Memory;
      segment   : in     T_SegmentIndex;
      index     : in     T_Word;
      byteIndex : in     T_Byte;
      value     : in     T_Byte) return T_Byte
   is
   begin
      put_internal(rwm, segment, index, byteIndex, value);
      return value;
   end put;

end RSmalltalk.Memory;
