with Interfaces; use Interfaces;

package RSmalltalk.Memory is

   subtype T_Byte is Unsigned_8;
   subtype T_Word is Unsigned_16;
   --subtype T_Int is Integer_16;
   subtype T_Int is Integer_16 range -(2**14 -1) .. 2**14;

   subtype T_Pointer is T_Word;

   C_NilPointer         : constant := 0;
   -- Any sixteen-bit value that cannot be an object table index
   C_NonPointer         : constant := T_Pointer'Last;
   
   -- segment index of the object table
   C_ObjectTableSegment : constant := 0;
   -- segment index of the begin of heap
   C_FirstHeapSegment   : constant := 1;
   -- segment count of heap
   C_HeapSegmentCount : constant := 2**4;
   -- segment index of the end of heap
   C_LastHeapSegment  : constant := (C_FirstHeapSegment + C_HeapSegmentCount - 1);


   subtype T_SegmentIndex is T_Byte range 0 .. (C_HeapSegmentCount - 1);
   
   type T_Memory(scount : T_SegmentIndex) is private;
   
   function isIntegerObject(ptr: T_Pointer) return Boolean;

   function integerValueOf (ptr : T_Pointer) return T_Int;

   function integerObjectOf (value : T_Int) return T_Pointer;

   function isIntegerValue (value : T_Word) return Boolean;
   
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
