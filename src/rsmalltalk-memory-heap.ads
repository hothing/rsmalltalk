package RSmalltalk.Memory.Heap is

   function getHeapChunkOf
     (mem           : T_Memory;
      objectPointer : T_Pointer;
      offset        : T_Word) return T_Word;

   procedure putHeapChunkOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      offset        : T_Word;
      value         : T_Word);

   function getHeapChunkByteOf
     (mem           : T_Memory;
      objectPointer : T_Pointer;
      offset        : T_Word) return T_Byte;

   procedure putHeapChunkByteOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      offset        : T_Word;
      value         : T_Byte);
   
   function getSizeFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word;

   procedure putSizeFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word);

   function getClassFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Pointer;

   procedure putClassFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      class         : T_Pointer);

end RSmalltalk.Memory.Heap;
