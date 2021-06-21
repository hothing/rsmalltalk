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

   function Word2IntObj is new Ada.Unchecked_Conversion(Source => T_Word,
                                                        Target => T_IntegerObject);

   function IntObj2Word is new Ada.Unchecked_Conversion(Source => T_IntegerObject,
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

   function isIntegerObject(ptr: T_Pointer) return Boolean
   is
   begin
      return asIntegerObject(ptr).int;
   end isIntegerObject;
   pragma Inline(isIntegerObject);

   function integerObjectOf (value : T_Int) return T_Pointer is
      nx : T_Word := Int2Word(value * 2);
      io : T_IntegerObject := Word2IntObj(nx);
   begin
      io.int := true;
      return T_Pointer(IntObj2Word(io));
   end integerObjectOf;
   pragma Inline(integerObjectOf);

   function integerObjectOf (value : T_Word) return T_Pointer is
      ni : T_Word := value * 2;
      io : T_IntegerObject;
      for io'Address use ni'Address;
   begin
      io.int := true;
      return T_Pointer(IntObj2Word(io));
   end integerObjectOf;
   pragma Inline(integerObjectOf);

   function asIntegerObject (value : T_Word) return T_IntegerObject is
   begin
      return Word2IntObj(value);
   end asIntegerObject;
   pragma Inline(asIntegerObject);

--     function integerValueOf (ptr : T_Pointer) return T_Int is
--     begin
--        return Word2Int(T_Word(asIntegerObject(ptr).val));
--     end integerValueOf;

   function integerValueOf (ptr : T_Pointer) return T_Int is
   begin
      return Word2Int(T_Word(ptr) and not 1) / 2;
   end integerValueOf;
   pragma Inline(integerValueOf);

   function wordValueOf (ptr : T_Pointer) return T_Word is
   begin
      return T_Word(asIntegerObject(ptr).val);
   end wordValueOf;
   pragma Inline(wordValueOf);

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
