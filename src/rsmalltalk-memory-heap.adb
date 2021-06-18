with RSmalltalk.Memory.ObjTbl; use RSmalltalk.Memory.ObjTbl;

package body RSmalltalk.Memory.Heap is

   -- the structre decalarations are descriptive,
   -- they should not use

   type T_MemoryArea is array(T_Pointer range <>) of T_Word;

   type T_ObjectStructure (fieldCount : T_Word) is record
      -- reference to the classobject
      class  : T_Pointer;
      -- instance variables or other data
      fields : T_MemoryArea (0 .. fieldCount);
   end record;

   -------------------

   subtype T_MethodHeaderFlags is T_Byte range 0..7;

   type T_MethodHeader is record
      smi     : Boolean;
      flags   : T_MethodHeaderFlags;
      tcount  : T_Counter_5;
      large   : Boolean;
      lcount  : T_Counter_6;
   end record;
   for T_MethodHeader use record
      smi     at 0 range 0 .. 0;
      lcount  at 0 range 1 .. 6;
      large   at 0 range 7 .. 7;
      tcount  at 0 range 8 .. 12;
      flags   at 0 range 13 .. 15;
   end record;
   for T_MethodHeader'Size use T_Word'Size;

   type T_MethodHeaderExtention is record
      smi     : Boolean;
      prim    : T_Byte; -- primitive index
      acount  : T_Counter_5; -- argument count
   end record;
   for T_MethodHeaderExtention use record
      smi     at 0 range 0 .. 0;
      prim    at 0 range 1 .. 8;
      acount  at 0 range 9 .. 13;
   end record;
   for T_MethodHeaderExtention'Size use T_Word'Size;

   MH_FLAG_ARG0 : constant := 0; -- no primitive and 0 argument
   MH_FLAG_ARG1 : constant := 1; -- no primitive and 1 argument
   MH_FLAG_ARG2 : constant := 2; -- no primitive and 2 argument
   MH_FLAG_ARG3 : constant := 3; -- no primitive and 3 argument
   MH_FLAG_ARG4 : constant := 4; -- no primitive and 4 argument
   MH_FLAG_RETURN_SELF : constant := 5; -- primitive and self return
   MH_FLAG_RETURN_IVAR : constant := 6; -- primitive and return of an instance variable
   MH_FLAG_EXTENTION : constant := 0; -- a header extention

   ------------------

   type T_InstanceSpecification is record
      smi     : Boolean;
      fcount  : T_Counter_11; -- fixed field count
      zero    : Boolean;
      indexable : Boolean; -- is indexable or structure
      words     : Boolean; -- storage unit is word or byte
      pointers  : Boolean; -- contains the pointers or numbers
   end record;
   for T_InstanceSpecification use record
      smi       at 0 range 0 .. 0;
      fcount    at 0 range 1 .. 11;
      zero      at 0 range 12 .. 12;
      indexable at 0 range 13 .. 13;
      words     at 0 range 14 .. 14;
      pointers  at 0 range 15 .. 15;
   end record;
   for T_InstanceSpecification'Size use T_Word'Size;

   --------------------
   -- getHeapChunkOf --
   --------------------

   function getHeapChunkOf
     (mem           : T_Memory;
      objectPointer : T_Pointer;
      offset        : T_Word)
      return T_Word
   is
   begin
--        heapChunkOf: objectPointer word: offset
--          ^wordMemory segment: (self segmentBitsOf: objectPointer)
--          word: ((self locationBitsOf: objectPointer) + offset)

      return get(mem.wordMemory,
                     getSegmentFieldOf(mem, objectPointer),
                     offset + getLocationFieldOf(mem, objectPointer)
                    );
   end getHeapChunkOf;

   --------------------
   -- putHeapChunkOf --
   --------------------

   procedure putHeapChunkOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      offset        : T_Word;
      value         : T_Word)
   is
   begin
      --        ^wordMemory segment: (self segmentBitsOf: objectPointer)
      --          word: ((self IocationBitsOf: objectPointer) + offset)
      --            put: value
      put(mem.wordMemory,
                     getSegmentFieldOf(mem, objectPointer),
                     offset + getLocationFieldOf(mem, objectPointer),
                     value
                    );
   end putHeapChunkOf;

   ------------------------
   -- getHeapChunkByteOf --
   ------------------------

   function getHeapChunkByteOf
     (mem           : T_Memory;
      objectPointer : T_Pointer;
      offset        : T_Word)
      return T_Byte
   is
   begin
      --        ^wordMemory segment: (self segmentBitsOf: objectPointer)
      --          word: ((self IocationBitsOf: objectPointer) + (offset // 2))
      --            byte: (offset \\ 2)
      return get(mem.wordMemory,
                     getSegmentFieldOf(mem, objectPointer),
                     (offset mod 2 ) + getLocationFieldOf(mem, objectPointer),
                     T_Byte(offset rem 2 )
                    );
   end getHeapChunkByteOf;

   ------------------------
   -- putHeapChunkByteOf --
   ------------------------

   procedure putHeapChunkByteOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      offset        : T_Word;
      value         : T_Byte)
   is
   begin
      put(mem.wordMemory,
                     getSegmentFieldOf(mem, objectPointer),
                     (offset mod 2 ) + getLocationFieldOf(mem, objectPointer),
                     T_Byte(offset rem 2 ),
                     value
                    );
   end putHeapChunkByteOf;

   -- object structure manipulation

   --------------------
   -- getSizeFieldOf --
   --------------------

   function getSizeFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return T_Word
   is
   begin
      -- ^self heapChunkOf: objectPointer word: 0
      return getHeapChunkOf(mem, objectPointer, 0);
   end getSizeFieldOf;

   --------------------
   -- putSizeFieldOf --
   --------------------

   procedure putSizeFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
   is
   begin
      putHeapChunkOf(mem, objectPointer, 0, value);
   end putSizeFieldOf;

   ---------------------
   -- getClassFieldOf --
   ---------------------

   function getClassFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return T_Pointer
   is
   begin
      -- ^self heapChunkOf: objectPointer word: 1
      return getHeapChunkOf(mem, objectPointer, 1);
   end getClassFieldOf;

   ---------------------
   -- putClassFieldOf --
   ---------------------

   procedure putClassFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      class         : T_Pointer)
   is
   begin
      putHeapChunkOf(mem, objectPointer, 1, T_Word(class));
   end putClassFieldOf;


end RSmalltalk.Memory.Heap;
