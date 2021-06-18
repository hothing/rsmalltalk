with Interfaces; use Interfaces;

package RSmalltalk.Memory is

   subtype T_Byte is Unsigned_8;
   subtype T_Word is Unsigned_16;
   --subtype T_Int is Integer_16;
   subtype T_Int is Integer_16 range -(2**14 -1) .. 2**14;

   subtype T_Pointer is T_Word;

   -- segment index of the object table
   C_ObjectTableSegment : constant := 0;
   -- segment index of the begin of heap
   C_FirstHeapSegment   : constant := 1;
   -- segment count of heap
   C_HeapSegmentCount : constant := 2**4;
   -- segment index of the end of heap
   C_LastHeapSegment  : constant := (C_FirstHeapSegment + C_HeapSegmentCount - 1);


   subtype T_SegmentIndex is T_Byte range 0 .. (C_HeapSegmentCount - 1);

   type T_Memory (scount : T_SegmentIndex) is private;

   type T_SegmentedMemory (count : T_SegmentIndex) is private;

   type T_ErrorHandler is access procedure(errId : T_Word);

   -- Object Table constants ^ declarations

   -- Object Table start offset(location)
   C_ObjectTableStart   : constant := 10;
   -- Object Table Size
   C_ObjectTableSize    : constant := T_Pointer'Last - C_ObjectTableStart;

   -- The current segment value
   C_CurrentSegment    : constant := 0;
   pragma Compile_Time_Warning(
                               C_ObjectTableStart <= C_CurrentSegment,
                               "Wrong offset of C_ObjectTableStart"
                              );

   -- The location of the head of linked list of free object table entries
   C_FreePointerList    : constant := C_CurrentSegment + 1;


   -- Object Header Size includes an object fields count and a class pointer
   C_HeaderSize       : constant := 2;

   -- BigSize is smallest size of chunk that is not stored in
   -- a list whose chunks are the same size
   C_BigSize            : constant := 20;

   C_HugeSize           : constant := 256;

   C_NilPointer         : constant := 0;
   -- Any sixteen-bit value that cannot be an object table index
   C_NonPointer         : constant := 65535;

   C_Class        : constant := 1;
   C_IntegerClass : constant := 2;
   C_MethodClass  : constant := 20;

   C_HeapSpaceStop    : constant := 0;

   -- The location of the head of linked list of free chunks of size 0
   C_FirstFreeChunkList : constant := 0;
   -- The location of the head of linked list of free chunks of
   -- size 'C_BigSize' or larger
   C_LastFreeChunkList  : constant := C_FirstFreeChunkList + C_BigSize;

   CE_NoError      : constant := 0;
   CE_NotObject    : constant := 1;
   CE_OutOfMemory  : constant := 2;

   function isIntegerObject(ptr: T_Pointer) return Boolean;

   function integerValueOf (ptr : T_Pointer) return T_Int;

   function integerObjectOf (value : T_Int) return T_Pointer;

   function isIntegerValue (value : T_Word) return Boolean;

   function get
     (rwm     : in T_SegmentedMemory;
      segment : in T_SegmentIndex;
      index   : in T_Word) return T_Word;

   function get
     (rwm       : in T_SegmentedMemory;
      segment   : in T_SegmentIndex;
      index     : in T_Word;
      byteIndex : in T_Byte) return T_Byte;

   function put
     (rwm     : in out T_SegmentedMemory;
      segment : in     T_SegmentIndex;
      index   : in     T_Word;
      value   : in     T_Word) return T_Word;

   function put
     (rwm       : in out T_SegmentedMemory;
      segment   : in     T_SegmentIndex;
      index     : in     T_Word;
      byteIndex : in     T_Byte;
      value     : in     T_Byte) return T_Byte;

   procedure put
     (rwm     : in out T_SegmentedMemory;
      segment : in     T_SegmentIndex;
      index   : in     T_Word;
      value   : in     T_Word);

   procedure put
     (rwm       : in out T_SegmentedMemory;
      segment   : in     T_SegmentIndex;
      index     : in     T_Word;
      byteIndex : in     T_Byte;
      value     : in     T_Byte);

   procedure setErrorHandler (mem : in out T_Memory;
                              ehandler : T_ErrorHandler);

private

   type T_MemorySegment is array(T_Pointer) of T_Word;

   type T_MemorySegments is array (T_SegmentIndex range <>) of T_MemorySegment;

   type T_SegmentedMemory (count : T_SegmentIndex) is record
      segments : T_MemorySegments (0 .. count);
   end record;

    type T_Memory (scount : T_SegmentIndex) is record -- this is RealObjectMemory
      wordMemory     : T_SegmentedMemory (scount);
      currentSegment : T_SegmentIndex;
      errorProc      : T_ErrorHandler;

   end record;

end RSmalltalk.Memory;
