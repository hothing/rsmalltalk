package body RSmalltalk.Memory.ObjTbl is

   ------------------
   -- getObjectRef --
   ------------------

   function getObjectRef
     (mem : T_Memory;
      objectPointer : T_Pointer)
      return T_Word
   is
   begin
      if isIntegerObject(objectPointer) then
         mem.errorProc(CE_NotObject);
         raise NonPointerException;
         return C_NonPointer;
      else
         return get(mem.wordMemory, C_ObjectTableSegment, C_ObjectTableStart + objectPointer);
      end if;
   end getObjectRef;

   ------------------
   -- putObjectRef --
   ------------------

   procedure putObjectRef
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
   is
   begin
      if isIntegerObject(objectPointer) then
         mem.errorProc(CE_NotObject);
         raise NonPointerException;
      else
         put(mem.wordMemory, C_ObjectTableSegment, C_ObjectTableStart + objectPointer, value);
      end if;
   end putObjectRef;

   ---------------------
   -- getCountFieldOf --
   ---------------------

   function getCountFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return T_Word
   is
      oeh : T_ObjectEntryHeader;
      x : T_Word;
      for x'Address use oeh'Address;
   begin
      x := getObjectRef(mem, objectPointer);
      return T_Word(oeh.count);
   end getCountFieldOf;

   ---------------------
   -- putCountFieldOf --
   ---------------------

   procedure putCountFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
   is
      b : T_Byte;
      oeh : T_ObjectEntryHeader;
      x : T_Word;
      for x'Address use oeh'Address;
   begin
      if value < C_HugeSize then
         b := T_Counter_8(value);
      else
         b := T_Counter_8'Last;
      end if;
      x := getObjectRef(mem, objectPointer);
      oeh.count := b;
      putObjectRef(mem, objectPointer, x);
   end putCountFieldOf;

   -------------------
   -- getOddFieldOf --
   -------------------

   function getOddFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return Boolean
   is
      oeh : T_ObjectEntryHeader;
      x : T_Word;
      for x'Address use oeh'Address;
   begin
      x := getObjectRef(mem, objectPointer);
      return oeh.o;
   end getOddFieldOf;

   -------------------
   -- putOddFieldOf --
   -------------------

   procedure putOddFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : Boolean)
   is
      oeh : T_ObjectEntryHeader;
      x : T_Word;
      for x'Address use oeh'Address;
   begin
      x := getObjectRef(mem, objectPointer);
      oeh.o := value;
      putObjectRef(mem, objectPointer, x);
   end putOddFieldOf;

   --------------------
   -- getFreeFieldOf --
   --------------------

   function getFreeFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return Boolean
   is
      oeh : T_ObjectEntryHeader;
      x : T_Word;
      for x'Address use oeh'Address;
   begin
      x := getObjectRef(mem, objectPointer);
      return oeh.f;
   end getFreeFieldOf;

   --------------------
   -- putFreeFieldOf --
   --------------------

   procedure putFreeFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : Boolean)
   is
      oeh : T_ObjectEntryHeader;
      x : T_Word;
      for x'Address use oeh'Address;
   begin
      x := getObjectRef(mem, objectPointer);
      oeh.f := value;
      putObjectRef(mem, objectPointer, x);
   end putFreeFieldOf;

   -------------------
   -- getPtrFieldOf --
   -------------------

   function getPtrFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return Boolean
   is
      oeh : T_ObjectEntryHeader;
      x : T_Word;
      for x'Address use oeh'Address;
   begin
      x := getObjectRef(mem, objectPointer);
      return oeh.p;
   end getPtrFieldOf;

   -------------------
   -- putPtrFieldOf --
   -------------------

   procedure putPtrFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : Boolean)
   is
      oeh : T_ObjectEntryHeader;
      x : T_Word;
      for x'Address use oeh'Address;
   begin
      x := getObjectRef(mem, objectPointer);
      oeh.p := value;
      putObjectRef(mem, objectPointer, x);
   end putPtrFieldOf;

   -----------------------
   -- getSegmentFieldOf --
   -----------------------

   function getSegmentFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return T_SegmentIndex
   is
      oeh : T_ObjectEntryHeader;
      x : T_Word;
      for x'Address use oeh'Address;
   begin
      x := getObjectRef(mem, objectPointer);
      return oeh.segment;
   end getSegmentFieldOf;

   -----------------------
   -- putSegmentFieldOf --
   -----------------------

   procedure putSegmentFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_SegmentIndex)
   is
      oeh : T_ObjectEntryHeader;
      x : T_Word;
      for x'Address use oeh'Address;
   begin
      x := getObjectRef(mem, objectPointer);
      oeh.segment := value;
      putObjectRef(mem, objectPointer, x);
   end putSegmentFieldOf;

   ------------------------
   -- getLocationFieldOf --
   ------------------------

   function getLocationFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return T_Word
   is
   begin
      return getObjectRef(mem, objectPointer + 1);
   end getLocationFieldOf;

   ------------------------
   -- putLocationFieldOf --
   ------------------------

   procedure putLocationFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
   is
   begin
      putObjectRef(mem, objectPointer + 1, value);
   end putLocationFieldOf;

   ------------------------------
   -- getHeadOfFreePointerList --
   ------------------------------

   function getHeadOfFreePointerList (mem : T_Memory) return T_Word is
   begin
      -- ^wordMemory segment: ObjectTableSegment word: FreePointerList
      return get(mem.wordMemory, C_ObjectTableSegment, C_FreePointerList);
   end getHeadOfFreePointerList;

   ------------------------------
   -- putHeadOfFreePointerList --
   ------------------------------

   procedure putHeadOfFreePointerList
     (mem           : in out T_Memory;
      objectPointer : T_Pointer)
   is
   begin
      put(mem.wordMemory,
                     C_ObjectTableSegment,
                     C_FreePointerList,
                     objectPointer);
   end putHeadOfFreePointerList;

   --------------------------
   -- addToFreePointerList --
   --------------------------

   procedure addToFreePointerList (mem : in out T_Memory; objectPointer : T_Pointer) is
   begin
      -- self locationBitsOf: objectPointer put: (self headOfFreePointerList).
      -- self headOfFreePointerListPut: objectPointer
      putLocationFieldOf(mem, objectPointer, getHeadOfFreePointerList(mem));
      putHeadOfFreePointerList(mem, objectPointer);
   end addToFreePointerList;

   -------------------------------
   -- removeFromFreePointerList --
   -------------------------------

   function removeFromFreePointerList (mem : in out T_Memory) return T_Pointer is
      op : T_Pointer;
   begin
      -- | objectPointer |
      -- objectPointer := self headOfFreePointerList.
      -- objectPointer = NonPointer ifTrue: [^nil].
      -- self headOfFreePointerListPut: (self LocationBitsOf: objectPointer).
      -- ^objectPointer
      op := getHeadOfFreePointerList(mem);
      if op /= C_NonPointer then
         putHeadOfFreePointerList(mem, getLocationFieldOf(mem, op));
      end if;
      return op;
   end removeFromFreePointerList;

   function getCurrentSegment (mem : T_Memory) return T_SegmentIndex
   is
      w : T_Word;
   begin
      w := get(mem.wordMemory, C_ObjectTableSegment, C_CurrentSegment);
      return T_SegmentIndex(w / 2);
   end getCurrentSegment;

   procedure putCurrentSegment
     (mem           : in out T_Memory;
      value : T_SegmentIndex)
   is
      w : T_Word;
   begin
      if isIntegerValue(T_Word(value)) then
         w := integerObjectOf(T_Int(value));
         put(mem.wordMemory,
             C_ObjectTableSegment,
             C_CurrentSegment,
             w);
      end if;
   end putCurrentSegment;

   procedure zeroReferenceCounts (mem : in out T_Memory) is
      ptr : T_Pointer;
   begin
      for idx in T_Pointer range T_Pointer'First .. (C_ObjectTableSize / 2) - 1
      loop
         ptr := idx * 2;
         putCountFieldOf(mem, ptr, 0);
      end loop;
   end zeroReferenceCounts;

   function obtainPointer(mem : in out T_Memory;
                          size : T_Word;
                          location : T_Word
                         ) return T_Pointer
   is
      ptr : T_Pointer;
   begin
      ptr := removeFromFreePointerList(mem);
      if ptr /= C_NilPointer then
         if ptr /= C_NonPointer then
            putObjectRef(mem, ptr, 0);
            putSegmentFieldOf(mem, ptr, getCurrentSegment(mem));
            putSizeFieldOf(mem, ptr, size);
            putLocationFieldOf(mem, ptr, location);
         else
            raise NonPointerException;
         end if;
      end if;
      return ptr;
   end obtainPointer;



end RSmalltalk.Memory.ObjTbl;
