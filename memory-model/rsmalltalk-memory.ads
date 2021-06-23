with Interfaces; use Interfaces;

package RSmalltalk.Memory is

   subtype T_Byte is Unsigned_8;
   subtype T_Word is Unsigned_16;
   subtype T_Int is Integer_16;
   
   subtype T_SmallWord is T_Word range T_Word'First .. T_Word'Last / 2;
   subtype T_SmallInt is Integer_16 range -2**14 .. 2**14 - 1;
   
   -- segment index of the object table
   C_ObjectTableSegment : constant := 0;
   -- segment index of the begin of heap
   C_FirstHeapSegment   : constant := 1;
   -- segment count of heap
   C_HeapSegmentCount : constant := 2**4;
   -- segment index of the end of heap
   C_LastHeapSegment  : constant := (C_FirstHeapSegment + C_HeapSegmentCount - 1);
   
   type T_NumericObject(int : Boolean) is record
      case int is
         when false => addr : T_SmallWord;
         when true => val : T_SmallInt;
      end case;
   end record;
   for T_NumericObject use record
      int  at 0 range 0 .. 0;
      addr at 0 range 1 .. 15;
      val  at 0 range 1 .. 15;
   end record;
   for T_NumericObject'Size use T_Word'Size;
   
   subtype T_Pointer is T_NumericObject(false);
   subtype T_IntObject is T_NumericObject(true);

   
   C_RawNilPointer      : constant := T_SmallWord'First;
   C_NilPointer         : constant T_Pointer := (int => false, addr => C_RawNilPointer);
   
   -- Any sixteen-bit value that cannot be an object table index
   C_RawNonPointer      : constant := T_SmallWord'Last;
   C_NonPointer         : constant T_Pointer := (int => false, addr => C_RawNonPointer);
   
   subtype T_SegmentIndex is T_Byte range 0 .. (C_HeapSegmentCount - 1);
   
   type T_Memory(scount : T_SegmentIndex) is private;

   function isSmallIntValue (value : T_Int) return Boolean;
   
   function isSmallWordValue (value : T_Word) return Boolean;
   
   function isIntegerObject(so: T_NumericObject) return Boolean;
   
   function isIntegerObject (value: T_Word) return Boolean;
   
   function integerValueOf (so : T_NumericObject) return T_Int;
      
   function wordValueOf (so : T_NumericObject) return T_Word;
   
   function addressOf (so : T_NumericObject) return T_Word;
   
   function rawValueOf (so : T_NumericObject) return T_Word;

   function integerObjectOf (value : T_Int) return T_IntObject;

   function wordObjectOf (value : T_Word) return T_IntObject;
   
   function asIntObject (value : T_Word) return T_IntObject;
   
   function asPointer (value : T_Word) return T_Pointer;
   
private
   
   type T_Segment is array (T_Word) of T_Word; 
   
   type T_SegmentArray is array (T_SegmentIndex range <>) of T_Segment;

   type T_Memory (scount : T_SegmentIndex) is record
      segment : T_SegmentArray (0 .. scount);
   end record;

   function get
     (rwm     : in T_Memory;
      segment : in T_SegmentIndex;
      index   : in T_Word) return T_Word;

   function get
     (rwm       : in T_Memory;
      segment   : in T_SegmentIndex;
      index     : in T_Word;
      byteIndex : in T_Byte) return T_Byte;

   function put
     (rwm     : in out T_Memory;
      segment : in     T_SegmentIndex;
      index   : in     T_Word;
      value   : in     T_Word) return T_Word;

   function put
     (rwm       : in out T_Memory;
      segment   : in     T_SegmentIndex;
      index     : in     T_Word;
      byteIndex : in     T_Byte;
      value     : in     T_Byte) return T_Byte;

   procedure put
     (rwm     : in out T_Memory;
      segment : in     T_SegmentIndex;
      index   : in     T_Word;
      value   : in     T_Word);

   procedure put
     (rwm       : in out T_Memory;
      segment   : in     T_SegmentIndex;
      index     : in     T_Word;
      byteIndex : in     T_Byte;
      value     : in     T_Byte);

   
end RSmalltalk.Memory;
