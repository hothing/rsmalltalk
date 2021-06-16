with Ada.Unchecked_Conversion;

package body RSmalltalk.Memory.Segmented is

   function W2BX is new Ada.Unchecked_Conversion(Source => T_Word,
                                                 Target => T_BytesInWord);

   function BX2W is new Ada.Unchecked_Conversion(Source => T_BytesInWord,
                                                 Target => T_Word);

   function get
     (rwm     : in T_SegmentedMemory;
      segment : in T_SegmentIndex;
      index   : in T_Word) return T_Word
   is
      x : T_Word;
      for x'Address use rwm.segments(segment)(index)'Address;
   begin
      return x;
   end get;


   function get
     (rwm       : in T_SegmentedMemory;
      segment   : in T_SegmentIndex;
      index     : in T_Word;
      byteIndex : in T_Byte) return T_Byte
   is
      x : T_BytesInWord;
      offs : T_Word := (index + T_Word(byteIndex / 2));
      for x'Address use rwm.segments(segment)(offs)'Address;
   begin
      case byteIndex mod 2 is
         when 0 => return x.al;
         when 1 => return x.ah;
         when others => return 0; -- never happens
      end case;
   end get;

   procedure put_internal
     (rwm     : in out T_SegmentedMemory;
      segment : in     T_SegmentIndex;
      index   : in     T_Word;
      value   : in     T_Word)
   is
      x : T_Word;
      for x'Address use rwm.segments(segment)(index)'Address;
   begin
      x := value;
   end put_internal;
   pragma Inline_Always(put_internal);

   procedure put_internal
     (rwm       : in out T_SegmentedMemory;
      segment   : in     T_SegmentIndex;
      index     : in     T_Word;
      byteIndex : in     T_Byte;
      value     : in     T_Byte)
   is
      x : T_BytesInWord;
      offs : T_Word := (index + T_Word(byteIndex / 2));
      for x'Address use rwm.segments(segment)(offs)'Address;
   begin
      case byteIndex mod 2 is
         when 0 => x.al := value;
         when 1 => x.ah := value;
         when others => null; -- never happens
      end case;
   end put_internal;
   pragma Inline_Always(put_internal);


   procedure put
     (rwm     : in out T_SegmentedMemory;
      segment : in     T_SegmentIndex;
      index   : in     T_Word;
      value   : in     T_Word)
   is
   begin
      put_internal(rwm, segment, index, value);
   end put;
   pragma Inline(put);

   procedure put
     (rwm       : in out T_SegmentedMemory;
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
     (rwm     : in out T_SegmentedMemory;
      segment : in     T_SegmentIndex;
      index   : in     T_Word;
      value   : in     T_Word) return T_Word
   is
   begin
      put_internal(rwm, segment, index, value);
      return value;
   end put;

   function put
     (rwm       : in out T_SegmentedMemory;
      segment   : in     T_SegmentIndex;
      index     : in     T_Word;
      byteIndex : in     T_Byte;
      value     : in     T_Byte) return T_Byte
   is
   begin
      put_internal(rwm, segment, index, byteIndex, value);
      return value;
   end put;

end RSmalltalk.Memory.Segmented;
