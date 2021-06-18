package body RSmalltalk.Memory.ObjTable is

   function isPointerValid(ptr : T_Pointer) return Boolean
   is
   begin
      return not isIntegerObject(ptr)
        and ptr >= C_OTStart;
   end isPointerValid;
   pragma Inline_Always(isPointerValid);

   function getHeader_unsafe (mem : T_Memory;
                       objectPointer : T_Pointer
                      ) return T_ObjectEntryHeader
   is
      p : T_Pointer := C_OTStart + objectPointer;
      w : T_Word;
      h : T_ObjectEntryHeader;
      for h'Address use w'Address;
   begin
      w := get(mem, C_ObjectTableSegment, p);
      return h;
   end getHeader_unsafe;
   pragma Inline_Always(getHeader_unsafe);

   function getHeader (mem : T_Memory;
                       objectPointer : T_Pointer
                      ) return T_ObjectEntryHeader
   is
      h : T_ObjectEntryHeader;
   begin
      if isPointerValid(objectPointer)
      then
         h := getHeader_unsafe(mem, objectPointer);
      else
         raise Wrong_Parameter_Exception;
      end if;
      return h;
   end getHeader;


   procedure putHeader_unsafe
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_ObjectEntryHeader)
   is
      p : T_Pointer := C_OTStart + objectPointer;
      w : T_Word;
      h : T_ObjectEntryHeader := value;
      for h'Address use w'Address;
   begin
      put(mem, C_ObjectTableSegment, p, w);
   end putHeader_unsafe;
   pragma Inline_Always(putHeader_unsafe);

   procedure putHeader
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_ObjectEntryHeader)
   is
   begin
      if isPointerValid(objectPointer) then
         putHeader_unsafe(mem, objectPointer, value);
      else
         raise Wrong_Parameter_Exception;
      end if;
   end putHeader;

   function getLocation_unsafe
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word
   is
      p : T_Pointer := C_OTStart + objectPointer + 1;
   begin
      return get(mem, C_ObjectTableSegment, p);
   end getLocation_unsafe;
   pragma Inline_Always(getLocation_unsafe);

   function getLocation
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word
   is
      p : T_Pointer := C_OTStart + objectPointer + 1;
   begin
      if isPointerValid(objectPointer) then
         return getLocation_unsafe(mem, objectPointer);
      else
         raise Wrong_Parameter_Exception;
         return 0;
      end if;
   end getLocation;

   procedure putLocation_unsafe
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
   is
      p : T_Pointer := C_OTStart + objectPointer + 1;
   begin
      put(mem, C_ObjectTableSegment, p, value);
   end putLocation_unsafe;
   pragma Inline_Always(putLocation_unsafe);

   procedure putLocation
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
   is
   begin
      if isPointerValid(objectPointer) then
         putLocation_unsafe(mem, objectPointer, value);
      else
         raise Wrong_Parameter_Exception;
      end if;
   end putLocation;

   function getNextFree
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Pointer
   is
      p : T_Pointer := objectPointer;
      h : T_ObjectEntryHeader;
   begin
      h := getHeader(mem, objectPointer);
      if h.free then
         p := getLocation(mem, objectPointer);
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
