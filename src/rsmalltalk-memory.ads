with Interfaces; use Interfaces;

package RSmalltalk.Memory is

   subtype T_Byte is Unsigned_8;
   subtype T_Word is Unsigned_16;

   subtype T_Pointer is T_Word;

   -- Heap constants ^ declarations
   HeapSegmentCount : constant := 2**4;
   FirstHeapSegment : constant := 1;
   LastHeapSegment  : constant := (FirstHeapSegment + HeapSegmentCount - 1);
   HeaderSize       : constant := 2;
   HeapSpaceStop    : constant := 0;

   subtype T_SegmentIndex is T_Byte range 0 .. (HeapSegmentCount - 1);

   -- Object Table constants ^ declarations
   ObjectTableSegment : constant := 0;
   ObjectTableStart   : constant := 0;
   ObjectTableSize    : constant := 65535;

   BigSize            : constant := 20;
   HugeSize           : constant := 256;

   NilPointer         : constant := 0;
   NonPointer         : constant := 65535;

   FreePointerList    : constant := 0;
   FirstFreeChunkList : constant := 0;
   LastFreeChunkList  : constant := FirstFreeChunkList + BigSize;

   subtype T_Int is Integer_16;
   subtype T_CompactSmallInteger is T_Int range -(2**14 -1) .. 2**14;
   type T_SmallInt is record
      smi   : Boolean;
      value : T_CompactSmallInteger;
   end record;

   for T_SmallInt use record
      smi   at 0 range 0 ..  0;
      value at 0 range 1 .. 15;
   end record;
   for T_SmallInt'Size use T_Word'Size;

   CE_NoError      : constant := 0;
   CE_NotObject    : constant := 1;
   CE_OutOfMemory  : constant := 2;


   C_Class        : constant := 1;
   C_IntegerClass : constant := 2;
   C_MethodClass  : constant := 20;


   -- ObjectMemory:ObjectTable
   function integerValueOf (ptr : T_Pointer) return T_Int;

   function integerObjectOf (value : T_Int) return T_Pointer;

   function isIntegerValue (value : T_Word) return Boolean;

end RSmalltalk.Memory;
