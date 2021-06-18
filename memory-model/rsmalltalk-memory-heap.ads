package RSmalltalk.Memory.Heap is

   C_BigSize : constant := 20;
   
   C_ObjectHeaderSize : constant := 2;
   
   C_FirstFreeChunkLocation : constant := 0;
   C_LastFreeChunkLocation : constant := C_FirstFreeChunkLocation + C_BigSize;
   
   Wrong_Parameter_Exception: exception;
   Wrong_Header_Exception: exception;
   
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
   
end RSmalltalk.Memory.Heap;
