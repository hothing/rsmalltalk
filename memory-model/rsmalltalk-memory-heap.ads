package RSmalltalk.Memory.Heap is

   C_BigSize : constant := 20;
   
   C_ObjectHeaderSize : constant := 2;
   
   C_FirstFreeChunkLocation : constant := 0;
   C_LastFreeChunkLocation : constant := C_FirstFreeChunkLocation + C_BigSize;
   
   Wrong_Parameter_Exception  : exception;
   Wrong_Header_Exception     : exception;
   Wrong_Address_Exception    : exception;
   
   procedure makeObjectHeader (mem : in out T_Memory;
                              seg : T_SegmentIndex; -- segment index
                              loc : T_Word; -- offset in memory
                              size : T_Word; -- size of object
                              classPtr : T_Pointer -- pointeer to the class (in object table)
                              );
   

   function testObjectHeader (mem : in out T_Memory;
                              seg : T_SegmentIndex; -- segment index
                              loc : T_Word -- offset in memory
                              ) return Boolean;
   
   function getObjectSize
     (mem           : T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word) return T_Word;

   procedure putObjectSize
     (mem           : in out T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word;
      size          : T_Word);

   function getObjectClass
     (mem           : T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word) return T_Pointer;

   procedure putObjectClass
     (mem           : in out T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word;
      class         : T_Pointer);

   function getObjectField
     (mem           : T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word;
      fieldIndex    : T_Word
     ) return T_Pointer;

   procedure putObjectField
     (mem           : in out T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word;
      fieldIndex    : T_Word;
      fieldObject   : T_Pointer);
   
   function isAddressValid(objectAddress : T_Word) return Boolean;
   
end RSmalltalk.Memory.Heap;
