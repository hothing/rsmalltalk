package body RSmalltalk.Memory.ObjTable is

   function getHeader (mem : T_Memory;
                          objectPointer : T_Pointer
                      ) return T_ObjectEntryHeader
   is
      p : T_Pointer := C_ObjectTableStart + objectPointer * 2;
      w : T_Word;
      h : T_ObjectEntryHeader;
      for h'Address use w'Address;
   begin
      w := get(mem.wordMemory, C_ObjectTableSegment, p);
      return h;
   end getHeader;


   procedure putHeader
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_ObjectEntryHeader)
   is
      p : T_Pointer := C_ObjectTableStart + objectPointer * 2;
      w : T_Word;
      h : T_ObjectEntryHeader := value;
      for h'Address use w'Address;
   begin
      put(mem.wordMemory, C_ObjectTableSegment, p, w);
   end putHeader;

   function getLocationOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word
   is
      p : T_Pointer := C_ObjectTableStart + objectPointer * 2 + 1;
   begin
      return get(mem.wordMemory, C_ObjectTableSegment, p);
   end getLocationOf;

   procedure putLocationOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
   is
      p : T_Pointer := C_ObjectTableStart + objectPointer * 2 + 1;
   begin
      put(mem.wordMemory, C_ObjectTableSegment, p, value);
   end putLocationOf;

   function getNextFree
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Pointer
   is
      p : T_Pointer := objectPointer;
      h : T_ObjectEntryHeader;
   begin
      h := getHeader(mem, objectPointer);
      if h.free then
         p := getLocationOf(mem, objectPointer);
         if p /=C_NonPointer then
            return T_Pointer(p);
         else
            return C_NilPointer;
         end if;
      else
         return C_NilPointer;
      end if;
   end getNextFree;

   procedure markAsFree(mem           : T_Memory;
                        objectPointer : T_Pointer)
   is
      p : T_Pointer := objectPointer;
      h : T_ObjectEntryHeader;
   begin
      h := getHeader(mem, objectPointer);
      if not h.free then
         h.count := 0;
         h.segment := T_SegmentIndex'First;
         h.free := true;
         h.ptr := false;
         h.odd := false;
      end if;
   end markAsFree;

end RSmalltalk.Memory.ObjTable;
