with Ada.Unchecked_Conversion;

package body RSmalltalk.Memory.ObjTable is

   function isPointerValid(mem : T_Memory; ptr : T_Pointer) return Boolean
   is
   begin
      return not isIntegerObject(ptr)
        and ptr.addr < (mem.segment(C_ObjectTableSegment)'Last - C_OTStart) / 2;
   end isPointerValid;
   pragma Inline(isPointerValid);

   function HeaderOffset(ptr : T_Pointer) return T_Word
   is
   begin
      return C_OTStart + rawValueOf(ptr);
   end HeaderOffset;
   pragma Inline_Always(HeaderOffset);

   function LocationOffset(ptr : T_Pointer) return T_Word
   is
   begin
      return HeaderOffset(ptr) + 1;
   end LocationOffset;
   pragma Inline_Always(LocationOffset);

   function W2OEH is new Ada.Unchecked_Conversion(Source => T_Word,
                                                  Target => T_ObjectEntryHeader);

   function OEH2W is new Ada.Unchecked_Conversion(Source => T_ObjectEntryHeader,
                                                  Target => T_Word);

   function getHeader_unsafe (mem : T_Memory;
                       objectPointer : T_Pointer
                      ) return T_ObjectEntryHeader
   is
      w : T_Word;
   begin
      w := get(mem, C_ObjectTableSegment, HeaderOffset(objectPointer));
      return W2OEH(w);
   end getHeader_unsafe;
   pragma Inline_Always(getHeader_unsafe);

   function getHeader (mem : T_Memory;
                       objectPointer : T_Pointer
                      ) return T_ObjectEntryHeader
   is
      h : T_ObjectEntryHeader;
   begin
      if isPointerValid(mem, objectPointer)
      then
         h := getHeader_unsafe(mem, objectPointer);
      else
         raise Wrong_Pointer_Format_Exception;
      end if;
      return h;
   end getHeader;


   procedure putHeader_unsafe
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_ObjectEntryHeader)
   is
   begin
      put(mem, C_ObjectTableSegment, HeaderOffset(objectPointer), OEH2W(value));
   end putHeader_unsafe;
   pragma Inline_Always(putHeader_unsafe);

   procedure putHeader
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_ObjectEntryHeader)
   is
   begin
      if isPointerValid(mem, objectPointer) then
         putHeader_unsafe(mem, objectPointer, value);
      else
         raise Wrong_Pointer_Format_Exception;
      end if;
   end putHeader;

   function getLocation_unsafe
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word
   is
   begin
      return get(mem, C_ObjectTableSegment, LocationOffset(objectPointer));
   end getLocation_unsafe;
   pragma Inline_Always(getLocation_unsafe);

   function getLocation
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word
   is
   begin
      if isPointerValid(mem, objectPointer) then
         return getLocation_unsafe(mem, objectPointer);
      else
         raise Wrong_Pointer_Format_Exception;
         return 0;
      end if;
   end getLocation;

   procedure putLocation_unsafe
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
   is
   begin
      put(mem, C_ObjectTableSegment, LocationOffset(objectPointer), value);
   end putLocation_unsafe;
   pragma Inline_Always(putLocation_unsafe);

   procedure putLocation
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
   is
   begin
      if isPointerValid(mem, objectPointer) then
         putLocation_unsafe(mem, objectPointer, value);
      else
         raise Wrong_Pointer_Format_Exception;
      end if;
   end putLocation;

   function getNextFree
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Pointer
   is
      p : T_Word;
      h : T_ObjectEntryHeader;
   begin
      h := getHeader(mem, objectPointer);
      if h.free then
         p := getLocation(mem, objectPointer);
         if p /= C_RawNonPointer then
            return (int => false, addr => p);
         else
            return C_NilPointer;
         end if;
      else
         return C_NilPointer;
      end if;
   end getNextFree;

   procedure markAsFree(mem           : in out T_Memory;
                        objectPointer : T_Pointer)
   is
      h : T_ObjectEntryHeader;
   begin
      --h := getHeader(mem, objectPointer);
      h.count := 0;
      if h.segment = C_ObjectTableSegment then
         h.segment := C_ObjectTableSegment + 1;
      end if;
      h.free := true;
      h.ptr := false;
      h.odd := false;
      putHeader(mem, objectPointer, h);
   end markAsFree;

   function getFreePointerListHead(mem  : in out T_Memory) return T_Pointer
   is
   begin
      return asPointer(get(mem,
          C_ObjectTableSegment,
          C_OTFreePointerList));
   end getFreePointerListHead;


   procedure putFreePointerListHead(mem           : in out T_Memory;
                                    objectPointer : T_Pointer)
   is
   begin
      put(mem,
          C_ObjectTableSegment,
          C_OTFreePointerList,
          rawValueOf(objectPointer));
   end putFreePointerListHead;

   procedure init(mem : in out T_Memory)
   is
      ifirst : T_Word := 1; -- it must be not NIL
      ilast  : T_Word := (mem.segment(C_ObjectTableSegment)'Last - C_OTStart);
      p1, p2 : T_Pointer;
      addr : T_Word := ((ilast - 1) / 2) * 2;
   begin
      p2 := C_NonPointer;
      -- 'addr' must be even
      if (addr and 1) = 1 then addr := addr - 1; end if;
      -- start interation over all entries
      while addr <= ilast and addr >= ifirst loop
         p1 := asPointer(addr);
         if p1 /= C_NilPointer then
            markAsFree(mem, p1);
            putLocation(mem, p1, rawValueOf(p2));
            p2 := p1;
         end if;
         addr := addr - 2;
      end loop;
      putFreePointerListHead(mem, p1);
   end init;

   function obtainPointer(mem : in out T_Memory) return T_Pointer
   is
      p1, p2 : T_Pointer ;
      w : T_Word;
   begin
      p1 := getFreePointerListHead(mem);
      if p1 /= C_NonPointer and p1 /= C_NilPointer then
         w := getLocation(mem, p1);
         p2 := asPointer(w);
         putFreePointerListHead(mem, p2);
         -- should be a location field re-initialized?
         -- ok, it will
         putLocation(mem, p1, C_RawNonPointer);
      end if;
      return p1;
   end obtainPointer;

end RSmalltalk.Memory.ObjTable;
