with Ada.Unchecked_Conversion;

with RSmalltalk.Memory.Segmented; use RSmalltalk.Memory.Segmented;

package body RSmalltalk.Memory.Objects is

   -------------------------
   -- cantBeIntegerObject --
   -------------------------

   procedure cantBeIntegerObject (mem : T_Memory; objectPointer : T_Pointer) is
   begin
      if isIntegerObject(objectPointer) then
         mem.errorProc(CE_NotObject);
      end if;
   end cantBeIntegerObject;

   ------------------
   -- getObjectRef --
   ------------------

   function getObjectRef
     (mem : T_Memory;
      objectPointer : T_Pointer)
      return T_Word
   is
   begin
      cantBeIntegerObject(mem, objectPointer);
      return get(mem.wordMemory, ObjectTableSegment, ObjectTableStart + objectPointer);
   end getObjectRef;

   ------------------
   -- putObjectRef --
   ------------------

   function putObjectRef
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
      return T_Word
   is
   begin
      cantBeIntegerObject(mem, objectPointer);
      return put(mem.wordMemory, ObjectTableSegment, ObjectTableStart + objectPointer, value);
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

   function putCountFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
      return T_Word
   is
      b : T_Byte;
      oeh : T_ObjectEntryHeader;
      x : T_Word;
      for x'Address use oeh'Address;
   begin
      if value < HugeSize then
         b := T_Counter_8(value);
      else
         b := 255;
      end if;
      x := getObjectRef(mem, objectPointer);
      oeh.count := b;
      return putObjectRef(mem, objectPointer, x);
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

   function putOddFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : Boolean)
      return T_Word
   is
      oeh : T_ObjectEntryHeader;
      x : T_Word;
      for x'Address use oeh'Address;
   begin
      x := getObjectRef(mem, objectPointer);
      oeh.o := value;
      return putObjectRef(mem, objectPointer, x);
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

   function putFreeFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : Boolean)
      return T_Word
   is
      oeh : T_ObjectEntryHeader;
      x : T_Word;
      for x'Address use oeh'Address;
   begin
      x := getObjectRef(mem, objectPointer);
      oeh.f := value;
      return putObjectRef(mem, objectPointer, x);
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

   function putPtrFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : Boolean)
      return T_Word
   is
      oeh : T_ObjectEntryHeader;
      x : T_Word;
      for x'Address use oeh'Address;
   begin
      x := getObjectRef(mem, objectPointer);
      oeh.p := value;
      return putObjectRef(mem, objectPointer, x);
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

   function putSegmentFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_SegmentIndex)
      return T_Word
   is
      oeh : T_ObjectEntryHeader;
      x : T_Word;
      for x'Address use oeh'Address;
   begin
      x := getObjectRef(mem, objectPointer);
      oeh.segment := value;
      return putObjectRef(mem, objectPointer, x);
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

   function putLocationFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
      return T_Word
   is
   begin
      return putObjectRef(mem, objectPointer, value);
   end putLocationFieldOf;

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

   function putHeapChunkOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      offset        : T_Word;
      value         : T_Word)
      return T_Word
   is
   begin
      --        ^wordMemory segment: (self segmentBitsOf: objectPointer)
      --          word: ((self IocationBitsOf: objectPointer) + offset)
      --            put: value
      return put(mem.wordMemory,
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

   function putHeapChunkByteOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      offset        : T_Word;
      value         : T_Byte)
      return T_Byte
   is
   begin
      return put(mem.wordMemory,
                     getSegmentFieldOf(mem, objectPointer),
                     (offset mod 2 ) + getLocationFieldOf(mem, objectPointer),
                     T_Byte(offset rem 2 ),
                     value
                    );
   end putHeapChunkByteOf;

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

   function putSizeFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
      return T_Word
   is
   begin
      return putHeapChunkOf(mem, objectPointer, 0, value);
   end putSizeFieldOf;

   ---------------------
   -- getClassFieldOf --
   ---------------------

   function getClassFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return T_Word
   is
   begin
      -- ^self heapChunkOf: objectPointer word: 1
      return getHeapChunkOf(mem, objectPointer, 1);
   end getClassFieldOf;

   ---------------------
   -- putClassFieldOf --
   ---------------------

   function putClassFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
      return T_Word
   is
   begin
      return putHeapChunkOf(mem, objectPointer, 1, value);
   end putClassFieldOf;

   ----------------------
   -- getLastPointerOf --
   ----------------------

   function getLastPointerOf
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return T_Word
   is
      methodHdr : T_Word;
   begin
      if getPtrFieldOf(mem, objectPointer) then
         if getClassFieldOf(mem, objectPointer) = C_MethodClass then
            methodHdr := getHeapChunkOf(mem, objectPointer, HeaderSize);
            return HeaderSize + 1 + (methodHdr and 126) / 2;
         else
            return getSizeFieldOf(mem, objectPointer);
         end if;
      else
         return HeaderSize;
      end if;
   end getLastPointerOf;

   ------------------------
   -- getSpaceOccupiedBy --
   ------------------------

   function getSpaceOccupiedBy
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return T_Word
   is
      size: T_Word;
   begin
      size := getSizeFieldOf(mem, objectPointer);
      if (size < HugeSize) or not getPtrFieldOf(mem, objectPointer) then
         return size;
      else
         return size + 1;
      end if;
   end getSpaceOccupiedBy;

   ------------------------------
   -- getHeadOfFreePointerList --
   ------------------------------

   function getHeadOfFreePointerList (mem : T_Memory) return T_Word is
   begin
      -- ^wordMemory segment: ObjectTableSegment word: FreePointerList
      return get(mem.wordMemory, ObjectTableSegment, FreePointerList);
   end getHeadOfFreePointerList;

   ------------------------------
   -- putHeadOfFreePointerList --
   ------------------------------

   function putHeadOfFreePointerList
     (mem           : in out T_Memory;
      objectPointer : T_Pointer)
      return T_Word
   is
   begin
      return put(mem.wordMemory,
                     ObjectTableSegment,
                     FreePointerList,
                     objectPointer);
   end putHeadOfFreePointerList;

   --------------------------
   -- addToFreePointerList --
   --------------------------

   procedure addToFreePointerList (mem : in out T_Memory; objectPointer : T_Pointer) is
      w : T_Word;
   begin
      -- self locationBitsOf: objectPointer put: (self headOfFreePointerList).
      -- self headOfFreePointerListPut: objectPointer
      w := putLocationFieldOf(mem, objectPointer, getHeadOfFreePointerList(mem));
      w := putHeadOfFreePointerList(mem, objectPointer);
   end addToFreePointerList;

   -------------------------------
   -- removeFromFreePointerList --
   -------------------------------

   function removeFromFreePointerList (mem : in out T_Memory) return T_Pointer is
      op : T_Pointer;
      w  : T_Word;
   begin
      -- | objectPointer |
      -- objectPointer := self headOfFreePointerList.
      -- objectPointer = NonPointer ifTrue: [^nil].
      -- self headOfFreePointerListPut: (self IocationBitsOf: objectPointer).
      -- ^objectPointer
      op := NilPointer;
      op := getHeadOfFreePointerList(mem);
      if op /= NonPointer then
         w := putHeadOfFreePointerList(mem, getLocationFieldOf(mem, op));
      end if;
      return op;
   end removeFromFreePointerList;

   ----------------------------
   -- getHeadOfFreeChunkList --
   ----------------------------

   function getHeadOfFreeChunkList
     (mem     : T_Memory;
      size    : T_Word;
      segment : T_SegmentIndex)
      return T_Word
   is
   begin
      -- ^wordMemory segment: segment word: FirstFreeChunkList + size
      return get(mem.wordMemory, segment, FirstFreeChunkList + size);
   end getHeadOfFreeChunkList;

   ----------------------------
   -- putHeadOfFreeChunkList --
   ----------------------------

   function putHeadOfFreeChunkList
     (mem           : in out T_Memory;
      size          : T_Word;
      segment       : T_SegmentIndex;
      objectPointer : T_Pointer)
      return T_Word
   is
   begin
      -- ^wordMemory segment: segment word: FirstFreeChunkList + size put: objectPointer
      return put(mem.wordMemory, segment, FirstFreeChunkList + size, objectPointer);
   end putHeadOfFreeChunkList;

   ------------------------
   -- addToFreeChunkList --
   ------------------------

   procedure addToFreeChunkList
     (mem           : in out T_Memory;
      size          : T_Word;
      objectPointer : T_Pointer)
   is
      seg : T_SegmentIndex;
      w : T_Word;
   begin
      --  | segment |
      -- segment ~ self segmentBitsOf: objectPointer.
      -- self classBitsOf: objectPointer put: (self headOfFreeChunkList: size inSegment: segment).
      -- self headOfFreeChunkList: size inSegment: segment put: objectPointer
      seg := getSegmentFieldOf(mem, objectPointer);
      w := putClassFieldOf(mem, objectPointer, getHeadOfFreeChunkList(mem, size, seg));
      w := putHeadOfFreeChunkList(mem, size, seg, objectPointer);
   end addToFreeChunkList;

   -----------------------------
   -- removeFromFreeChunkList --
   -----------------------------

   function removeFromFreeChunkList (mem : in out T_Memory; size : T_Word) return T_Pointer is
      op : T_Pointer;
      w : T_Word;
   begin
      --  | objectPointer secondChunk |
      -- objectPointer ~ self headOfFreeChunkList: size inSegment: currentSegment.
      -- objectPointer = NonPointer ifTrue [^nil].
      -- secondChunk ~ self classBitsOf: objectPointer.
      -- self headOfFreeChunkList: size inSegment: currentSegment put: secondChunk.
      -- ^objectPointer
      op := getHeadOfFreeChunkList(mem, size, mem.currentSegment);
      if op /= NonPointer then
         w := getClassFieldOf(mem, op);
         w := putHeadOfFreeChunkList(mem, size, mem.currentSegment, w);
      else
         op := NilPointer;
      end if;
      return op;
   end removeFromFreeChunkList;

   ------------------------
   -- resetFreeChunkList --
   ------------------------

   procedure resetFreeChunkList
     (mem     : in out T_Memory;
      size    : T_Word;
      segment : T_SegmentIndex)
   is
      w : T_Word;
   begin
      --  self headOfFreeChunkList: size inSegment: segment put: NonPointer
      w := putHeadOfFreeChunkList(mem, size, segment, NonPointer);
   end resetFreeChunkList;

   --------------
   -- allocate --
   --------------

   function allocate (
                      mem : in out T_Memory;
                      size : T_Word;
                      classPointer : T_Pointer) return T_Pointer is
      op : T_Pointer;
      w : T_Word;
   begin
      -- " ** Preliminary Version ** "
      --  | objectPointer |
      --  objectPointer ~ self allocateChunk: size. " actually allocate "
      --  self classBitsOf: objectPointer put: classPointer. " fill in class"
      --  " initialize all fields to the object table index of the object nil"
      --  (headerSize to: size-1) do:
      --  	[ :i | self heapChunkOf: objectPointer word: i put: NilPointer].
      --  self sizeBitsOf: objectPointer put: size.
      --  "return the new object's pointer"
      --  ^objectPointer
      op := allocateChunk(mem, size);
      w := putClassFieldOf(mem, op, classPointer);
      for i in HeaderSize .. (size - 1) loop
         w := putHeapChunkOf(mem, op, i, NilPointer);
      end loop;
      return op;
   end allocate;

   -------------------
   -- allocateChunk --
   -------------------

   function allocateChunk (mem : in out T_Memory; size : T_Word) return T_Pointer is
      op : T_Pointer;
   begin
      --        ** Preliminary Version **
      --        | objectPointer |
      --        objectPointer ~ self attemptToAllocateChunk: size.
      --        objectPointer isNil ifFalse: [^objectPointer].
      --        ^self error: 'Out of memory'
      op := attemptToAllocateChunk(mem, size);
      if op = NilPointer then
         -- garbage collect and try again
         reclaimInaccessibleObjects(mem);
         op := attemptToAllocateChunk(mem, size);
         if op = NilPointer then mem.errorProc(CE_OutOfMemory); end if;
      end if;
      return op;
   end allocateChunk;

   ----------------------------
   -- attemptToAllocateChunk --
   ----------------------------

   function attemptToAllocateChunk
     (mem  : in out T_Memory;
      size : T_Word)
      return T_Pointer
   is
      op : T_Pointer;
   begin
      --  | objectPointer |
      --  objectPointer ~ self attemptToAllocateChunklnCurrentSegment: size.
      --  objectPointer isNil ifFalse: [^objectPointer].
      --  1 to: HeapSegmentCount do:
      --    [:i |
      --       currentSegment ~ currentSegment - 1.
      --       currentSegment > LastHeapSegment ifTrue: [currentSegment ~ FirstHeapSegment].
      --       self compactCurrentSegment.
      --       objectPointer ~ self attemptToAIIocateChunklnCurrentSegment: size.
      --       objectPointer isNil ifFalse: [^objectPointer].
      --    ]
      --  ^nil
      op := attemptToAllocateChunkInCurrentSegment(mem, size);
      if op = NilPointer then
         for i in 1 .. HeapSegmentCount loop
            mem.currentSegment := mem.currentSegment - 1;
            if mem.currentSegment > LastHeapSegment then
               mem.currentSegment := FirstHeapSegment;
            end if;
            compactCurrentSegment(mem);
            op := attemptToAllocateChunkInCurrentSegment(mem, size);
            exit when op /= NilPointer;
         end loop;
      end if;
      return op;
   end attemptToAllocateChunk;

   --------------------------------------------
   -- attemptToAllocateChunkInCurrentSegment --
   --------------------------------------------

   function attemptToAllocateChunkInCurrentSegment
     (mem  : in out T_Memory;
      size : T_Word)
      return T_Pointer
   is
      objectPointer, newPointer, predecessor, next : T_Pointer;
      availableSize, excessSize, w : T_Word;
   begin
      if size < BigSize then --- small chunk of exact size handy so use it
         objectPointer := removeFromFreeChunkList(mem, size);
      end if;

      if objectPointer = NilPointer then
         predecessor := NonPointer;
         --- remember predecessor of chunk under consideration
         objectPointer := getHeadOfFreeChunkList(mem, LastFreeChunkList, mem.currentSegment);
         --- the search loop stops when the end of the linked list is encountered
         while objectPointer /= NonPointer loop
            availableSize := getSizeFieldOf(mem, objectPointer);
            if availableSize = size then --- exact fit --remove from free chunk list and return
               next := getClassFieldOf(mem, objectPointer); --- the link to the next chunk
               if predecessor = NonPointer then
                  w := putHeadOfFreeChunkList(mem, LastFreeChunkList, mem.currentSegment, next);
               else --- it was between two chunks; link them together
                  w := putClassFieldOf(mem, predecessor, next);
                  exit; --!! TO REWORK
               end if;
            else
               excessSize := availableSize - size;
               if excessSize >= HeaderSize then
                  --- can be broken into two usable parts: return the second part
                  --- obtain an object table entry for the second part
                  newPointer := obtainPointer(mem, size, getLocationFieldOf(mem, objectPointer) + excessSize);
                  if newPointer /= NilPointer then
                     --- correct the size of the first part (which remains on the free list)
                     w := putSizeFieldOf(mem, objectPointer, excessSize);
                  end if;
                  objectPointer := newPointer;
                  exit; --!! TO REWORK
               else
                  --- not big enough to use; try the next chunk on the list
                  predecessor := objectPointer;
                  objectPointer := getClassFieldOf(mem, objectPointer);
               end if;
            end if;
         end loop;
         --- the end of the linked list was reached and no fit was found
      end if;
      return objectPointer;
   end attemptToAllocateChunkInCurrentSegment;

   -------------------
   -- obtainPointer --
   -------------------

   function obtainPointer
     (mem       : in out T_Memory;
      size      : T_Word;
      location : T_Word)
      return T_Pointer
   is
      objectPointer : T_Pointer;
      w : T_Word;
   begin
      objectPointer := removeFromFreeChunkList(mem, size);
      if objectPointer /= NilPointer then
         w := putObjectRef(mem, objectPointer, 0);
         w := putSegmentFieldOf(mem, objectPointer, mem.currentSegment);
         w := putLocationFieldOf(mem, objectPointer, location);
         w := putSizeFieldOf(mem, objectPointer, size);
      end if;
      return objectPointer;
   end obtainPointer;

   ----------------
   -- deallocate --
   ----------------

   procedure deallocate (mem : in out T_Memory; classPointer : T_Pointer) is
      space : T_Word;
   begin
      space := getSpaceOccupiedBy(mem, classPointer);
--        pragma Compile_Time_Warning (Standard.True, "deallocate unimplemented");
--        raise Program_Error with "Unimplemented function deallocate";
      addToFreeChunkList(mem, T_Word'Min(space, BigSize), classPointer);
   end deallocate;

   --------------------------------
   -- abandonFreeChunkslnSegment --
   --------------------------------

   function abandonFreeChunkslnSegment
     (mem     : in out T_Memory;
      segment : T_SegmentIndex)
      return T_Word
   is
      lowWaterMark, w : T_Word;
      objectPointer, nextPointer : T_Pointer;
   begin
      lowWaterMark := HeapSpaceStop; --- first assume that no chunk is free
      declare
         subtype T_Counter is T_Word range HeaderSize .. BigSize;
      begin
         for size in T_Counter loop --- for each free-chunk list
            objectPointer := getHeadOfFreeChunkList(mem, size, segment);
            while objectPointer /= NonPointer loop
               lowWaterMark := T_Word'Min(lowWaterMark, getLocationFieldOf(mem, objectPointer));
               nextPointer := getClassFieldOf(mem, objectPointer);
               --- link to next free chunk
               w := putClassFieldOf(mem, objectPointer, NonPointer);
               --- distinguish for sweep
               releasePointer(mem, objectPointer);
               --- add entry to free-pointer list
               objectPointer := nextPointer;
            end loop;
            resetFreeChunkList(mem, size, segment);
         end loop;
      end;
      return lowWaterMark;
   end abandonFreeChunkslnSegment;

   --------------------
   -- releasePointer --
   --------------------

   procedure releasePointer (mem : in out T_Memory; ptr : T_Pointer) is
      w : T_Word;
   begin
      w := putFreeFieldOf(mem, ptr, True);
      addToFreePointerList(mem, ptr);
   end releasePointer;

   ------------------------------
   -- reverseHeapPointersAbove --
   ------------------------------

   procedure reverseHeapPointersAbove (mem : in out T_Memory; lowWaterMark : T_Word) is
      size, w : T_Word;
      objectPointer : T_Pointer;
   begin
      objectPointer := 0;
      while objectPointer < (ObjectTableSize - 2) loop
         if not getFreeFieldOf(mem, objectPointer) then
            --- the Object Table entry is in use
            if getSegmentFieldOf(mem, objectPointer) = mem.currentSegment then
               --- the object is in this segment
               if getLocationFieldOf(mem, objectPointer) >= lowWaterMark then
                  --- the object will be swept
                  size := getSizeFieldOf(mem, objectPointer); --- rescue the size
                  w := putSizeFieldOf(mem, objectPointer, objectPointer); --- reverse the pointer
                  w := putLocationFieldOf(mem, objectPointer, size); --- save the size
               end if;
            end if;
         end if;
         objectPointer := objectPointer + 2;
      end loop;
   end reverseHeapPointersAbove;

   -----------------------------
   -- sweepCurrentSegmentFrom --
   -----------------------------

   function sweepCurrentSegmentFrom (mem : in out T_Memory; lowWaterMark : T_Word) return T_Word is
      size, w, si, di: T_Word;
      objectPointer : T_Pointer;
   begin
      si := lowWaterMark;
      di := si;
      while si < HeapSpaceStop loop --- for each object, si
         if get(mem.wordMemory, mem.currentSegment, si + 1) = NonPointer then
            --- unallocated, so skip it
            size := get(mem.wordMemory, mem.currentSegment, si);
            si := si + size;
         else
            --- allocated, so keep it, but move it to compact storage
            objectPointer := get(mem.wordMemory, mem.currentSegment, si);
            size := getLocationFieldOf(mem, objectPointer);
            --- the reversed size
            w := putLocationFieldOf(mem, objectPointer, di); --- point object table at new location
            w := putSizeFieldOf(mem, objectPointer, size); --- restore the size to its proper place
            --- skip the size
            si := si + 1; di := di + 1;
            --- move the rest of the object
            for i in 2 .. getSpaceOccupiedBy(mem, objectPointer) loop
               w := get(mem.wordMemory, mem.currentSegment, si);
               put(mem.wordMemory, mem.currentSegment, di, w);
               si := si + 1; di := di + 1;
            end loop;
         end if;
      end loop;
      return di;
   end sweepCurrentSegmentFrom;

   ---------------------------
   -- compactCurrentSegment --
   ---------------------------

   procedure compactCurrentSegment (mem : in out T_Memory) is
      lowWaterMark, bigSpace : T_Word;
   begin
      lowWaterMark := abandonFreeChunkslnSegment(mem, mem.currentSegment);
      if lowWaterMark < HeapSpaceStop then
         reverseHeapPointersAbove(mem, lowWaterMark);
         bigSpace := sweepCurrentSegmentFrom(mem, lowWaterMark);
         deallocate(mem, obtainPointer(mem, HeapSpaceStop + 1 - bigSpace, bigSpace));
      end if;
   end compactCurrentSegment;

   ---------------------------
   --*  Garbage collection *--
   ---------------------------

--     function forAllOtherObjectsAccessibleFrom(mem : in out T_Memory; objectPointer: in out T_Pointer; predicate : T_Predicat; action : T_Action) return T_Pointer is
--        w, offset : T_Word;
--        next, p : T_Pointer;
--     begin
--        w := getLastPointerOf(mem, objectPointer) - 1;
--        offset := 0;
--        while offset < w loop
--           next := getHeapChunkOf(mem, objectPointer, offset);
--           if not isIntegerObject(mem, next) and predicate(mem, next) then
--              --- it's a non-immediate object and it should be processed
--              p := forAllOtherObjectsAccessibleFrom(mem, next, predicate, action);
--           end if;
--           offset := offset + 1;
--        end loop;
--        --- all pointers have been followed; now perform the action
--        action(mem, objectPointer);
--        return objectPointer;
--     end forAllOtherObjectsAccessibleFrom;
--
--     function forAllObjectsAccessibleFrom(mem : in out T_Memory; objectPointer: in out T_Pointer; predicate : T_Predicat; action : T_Action) return T_Pointer is
--        p : T_Pointer;
--     begin
--        p := objectPointer;
--        if not isIntegerObject(mem, p) and predicate(mem, p) then
--           --- it's a non-immediate object and it should be processed
--           p := forAllOtherObjectsAccessibleFrom(mem, p, predicate, action);
--        end if;
--        return p;
--     end forAllObjectsAccessibleFrom;

   -------------
   -- countUp --
   -------------

   function countUp
     (mem : in out T_Memory;
      objectPointer : T_Pointer)
      return T_Pointer
   is
      count, w : T_Word;
   begin
      if not isIntegerObject(objectPointer) then
         count := getCountFieldOf(mem, objectPointer);
         if count <= 128 then
            w := putCountFieldOf(mem, objectPointer, count + 1);
         end if;
      end if;
      return objectPointer;
   end countUp;

   ---------------
   -- countDown --
   ---------------

   function countDown_predicat(mem : in out T_Memory; value: T_Word) return Boolean is
      count : T_Word;
   begin
      count := getCountFieldOf(mem, value) - 1;
      if count < 127 then
         count := putCountFieldOf(mem, value, count);
      end if;
      return count = 0;
   end countDown_predicat;

   procedure countDown_action(mem : in out T_Memory; value: in out T_Word) is
   begin
      null;
   end countDown_action;

   function countDown
     (mem : in out T_Memory;
      objectPointer : T_Pointer)
      return T_Pointer
   is
      p : T_Pointer;
   begin
      pragma Compile_Time_Warning(True, "[countDown] is not implemented");
      p := objectPointer;
      if not isIntegerObject(objectPointer) then
         --- the predicate decrements the count and tests for zero
         --- the action zeroes the count and deallocates the object
         --FIXME: p := forAllObjectsAccessibleFrom(mem, p, countDown_predicat'Access, countDown_action'Access);
         null;
         --FIXME:END
      end if;
      return p;
   end countDown;

   --------------------------------
   -- reclaimlnaccessibleObjects --
   --------------------------------

   procedure reclaimInaccessibleObjects (mem : in out T_Memory) is
   begin
      zeroReferenceCounts(mem);
      markAccessibleObjects(mem);
      rectifyCountsAndDeallocateGarbage(mem);
   end reclaiminaccessibleobjects;

   -------------------------
   -- zeroReferenceCounts --
   -------------------------

   procedure zeroReferenceCounts (mem : in out T_Memory) is
      offset, w : T_Word;
   begin
      offset := 0;
      while offset < ObjectTableSize - 2 loop
         w := putCountFieldOf(mem, offset, 0);
         -- WARNING! This code is unclear.
         -- I dont understand why use offset in object table without convertion
         offset := offset + 2;
      end loop;
   end zeroReferenceCounts;

   -------------------------------
   -- markObjectsAccessibleFrom --
   -------------------------------

   function markOAF_predicat(mem : in out T_Memory; value: T_Word) return Boolean is
   begin
      --- the predicate tests for an unmarked object
      return getCountFieldOf(mem, value) = 0;
   end markOAF_predicat;

   procedure markOAF_action(mem : in out T_Memory; value: in out T_Word) is
      w : T_Word;
   begin
      w := putCountFieldOf(mem, value, 1);
   end markOAF_action;

   function markObjectsAccessibleFrom(mem: in out T_Memory; objectPointer : in out T_Pointer) return T_Pointer is

   begin
      pragma Compile_Time_Warning(True, "[markObjectsAccessibleFrom] is not implemented");
      --return forAllObjectsAccessibleFrom(mem, objectPointer, countDown_predicat'Access, countDown_action'Access);
      return 0;
   end markObjectsAccessibleFrom;


--     function getRootObjects(mem : T_Memory) return T_WordsSeq_Ptr is
--     begin
--        pragma Compile_Time_Warning (Standard.True, "getRootObjects unimplemented");
--        raise Program_Error with "Unimplemented function getRootObjects";
--        return getRootObjects (mem => mem);
--     end getRootObjects;
--
--     function getRootObjectsList(mem : T_Memory) return T_WordsList_Ptr is
--     begin
--        pragma Compile_Time_Warning (Standard.True, "getRootObjectsList unimplemented");
--        raise Program_Error with "Unimplemented function getRootObjectsList";
--        return getRootObjectsList (mem => mem);
--     end getRootObjectsList;

   ---------------------------
   -- markAccessibleObjects --
   ---------------------------
   procedure markAccessibleObjects (mem : in out T_Memory) is
   begin
      pragma Compile_Time_Warning (Standard.True, "[markAccessibleObjects] unimplemented");
      ---- rootObjectPointers do: [ :rootObjectPointer | self markObjectsAccessibleFrom: rootObjectPointer]
--        for w in getRootObjects(mem).all'Range loop
--           p := w;
--           n := markObjectsAccessibleFrom(mem, p);
--        end loop;
   end markAccessibleObjects;

   ---------------------------------------
   -- rectifyCountsAndDeallocateGarbage --
   ---------------------------------------

   procedure rectifyCountsAndDeallocateGarbage (mem : in out T_Memory) is
      count, s : T_Word;
      seg : T_SegmentIndex;
      p, p2, p3, p4 : T_Pointer;
   begin
      --- reset heads of free-chunk lists
      seg := FirstHeapSegment;
      while seg < LastHeapSegment loop --- for every segment
         s := HeaderSize;
         while s < BigSize loop
            --- reset the list head
            resetFreeChunkList(mem, s, seg);
            s := s + 1;
         end loop;
         seg := seg + 1;
      end loop;
      --- rectify counts, and deallocate garbage
      p := 0;
      while p < ObjectTableSize - 2 loop --- for every object table entry
         if not getFreeFieldOf(mem, p) then --- if it is not a free entry
            count := getCountFieldOf(mem, p);
            if count = 0 then --- it is unmarked, so deallocate it
               deallocate(mem, p);
            else --- it is marked, so rectify reference counts
               if count < 128 then ---  subtract 1 to compensate for the mark
                  s := putCountFieldOf(mem, p, count - 1);
               end if;
               p2 := 1;
               p3 := getLastPointerOf(mem, p) - 1;
               while p2 <= p3 loop --- increment the reference count of each pointer
                  p4 := countUp(mem, getHeapChunkOf(mem, p, p2));
               end loop;
            end if;
         end if;
         p := p + 2;
      end loop;
      --- be sure the root objects don't disappear
      pragma Compile_Time_Warning (Standard.True, "[rectifyCountsAndDeallocateGarbage] is not fully implemented");
--        for w in getRootObjects(mem).all'Range loop
--           p := w;
--           p2 := countUp(mem, p);
--        end loop;
   end rectifyCountsAndDeallocateGarbage;

   ----------------------
   -- allocateNPObject --
   ----------------------

   function allocate
     (mem          : in out T_Memory;
      size         : T_Word;
      odd          : Boolean;
      ptr          : Boolean;
      extraWord    : T_Word;
      classPointer : T_Pointer)
      return T_Pointer
   is
      op, default : T_Pointer;
      w : T_Word;
   begin
      w := countUp(mem, classPointer);
      op := allocateChunk(mem, size + (extraWord mod 2));
      w := putClassFieldOf(mem, op, classPointer);
      w := putOddFieldOf(mem, op, odd);
      w := putPtrFieldOf(mem, op, ptr);
      default := NilPointer;
      if (not ptr) then default := 0; end if;
      for i in HeaderSize .. size - 1 loop
         w := putHeapChunkOf(mem, op, T_Word(i), default);
      end loop;
      w := putSizeFieldOf(mem, op, size);
      return op;
   end allocate;

   ------------------
   -- fetchPointer --
   ------------------

   function fetchPointer
     (mem           : T_Memory;
      fieldIndex    : T_Word;
      objectPointer : T_Pointer)
      return T_Pointer
   is
   begin
      return getHeapChunkOf(mem, objectPointer, HeaderSize + fieldIndex);
   end fetchPointer;

   ------------------
   -- storePointer --
   ------------------

   function storePointer
     (mem           : in out T_Memory;
      fieldIndex    : T_Word;
      objectPointer : T_Pointer;
      valuePointer  : T_Word) return T_Word
   is
      chnk, w : T_Word;
   begin
      chnk := HeaderSize + fieldIndex;
      w := countUp(mem, valuePointer);
      w := countDown(mem, getHeapChunkOf(mem, objectPointer, chnk));
      return putHeapChunkOf(mem, objectPointer, chnk, valuePointer);
   end storePointer;

   ---------------
   -- fetchWord --
   ---------------

   function fetchWord
     (mem           : T_Memory;
      wordIndex     : T_Word;
      objectPointer : T_Pointer)
      return T_Word
   is
   begin
      return getHeapChunkOf(mem, objectPointer, HeaderSize + wordIndex);
   end fetchWord;

   ---------------
   -- storeWord --
   ---------------

   function storeWord
     (mem           : in out T_Memory;
      wordIndex     : T_Word;
      objectPointer : T_Pointer;
      valueWord     : T_Word) return T_Word
   is
   begin
      return putHeapChunkOf(mem, objectPointer, HeaderSize, valueWord);
   end storeWord;

   ---------------
   -- fetchByte --
   ---------------

   function fetchByte
     (mem           : T_Memory;
      byteIndex     : T_Word;
      objectPointer : T_Pointer)
      return T_Byte
   is
   begin
      return getHeapChunkByteOf(mem, objectPointer, HeaderSize * 2 + byteIndex);
   end fetchByte;

   ---------------
   -- storeByte --
   ---------------

   function storeByte
     (mem           : in out T_Memory;
      byteIndex     : T_Word;
      objectPointer : T_Pointer;
      valueByte     : T_Byte) return T_Byte
   is
   begin
      return putHeapChunkByteOf(mem, objectPointer, HeaderSize * 2 + byteIndex, valueByte);
   end storeByte;

   --------------------------
   -- increaseReferencesTo --
   --------------------------

   procedure increaseReferencesTo (mem : in out T_Memory; objectPointer : T_Pointer) is
      w : T_Word;
   begin
      w := countUp(mem, objectPointer);
   end increaseReferencesTo;

   --------------------------
   -- decreaseReferencesTo --
   --------------------------

   procedure decreaseReferencesTo (mem : in out T_Memory; objectPointer : T_Pointer) is
      w : T_Word;
   begin
      w := countDown(mem, objectPointer);
   end decreaseReferencesTo;

   ------------------
   -- fetchClassOf --
   ------------------

   function fetchClassOf
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return T_Pointer
   is
   begin
      if isIntegerObject(objectPointer) then
         return C_IntegerClass;
      else
         return getClassFieldOf(mem, objectPointer);
      end if;
   end fetchClassOf;

   -----------------------
   -- fetchWordLengthOf --
   -----------------------

   function fetchWordLengthOf
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return T_Word
   is
   begin
      return getSizeFieldOf(mem, objectPointer) - HeaderSize;
   end fetchWordLengthOf;

   -----------------------
   -- fetchByteLengthOf --
   -----------------------

   function fetchByteLengthOf
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return T_Word
   is
      w : T_Word;
   begin
      w := 0;
      if getOddFieldOf(mem, objectPointer) then w := 1; end if;
      return fetchWordLengthOf(mem, objectPointer) * 2 - w;
   end fetchByteLengthOf;

   ----------------------------------
   -- instantiateClassWithPointers --
   ----------------------------------

   function instantiateClassWithPointers
     (mem          : in out T_Memory;
      classPointer : T_Pointer;
      length       : T_Word)
      return T_Pointer
   is
      size, extra: T_Word;
   begin
      size := HeaderSize + length;
      if size < HugeSize then extra := 0;
      else extra := 1;
      end if;
      return allocate(mem, size, False, True, extra, classPointer);
   end instantiateClassWithPointers;

   -------------------------------
   -- instantiateClassWithWords --
   -------------------------------

   function instantiateClassWithWords
     (mem          : in out T_Memory;
      classPointer : T_Pointer;
      length       : T_Word)
      return T_Pointer
   is
      size: T_Word;
   begin
      size := HeaderSize + length;
      return allocate(mem, size, False, False, 0, classPointer);
   end instantiateClassWithWords;

   -------------------------------
   -- instantiateClassWithBytes --
   -------------------------------

   function instantiateClassWithBytes
     (mem          : in out T_Memory;
      classPointer : T_Pointer;
      length       : T_Word)
      return T_Pointer
   is
      size: T_Word;
   begin
      size := HeaderSize + (length + 1) / 2;
      return allocate(mem, size, ((length rem 2) = 1), False, 0, classPointer);
   end instantiateClassWithBytes;

   -----------------------
   -- initialInstanceOf --
   -----------------------

   function initialInstanceOf
     (mem          : in out T_Memory;
      classPointer : T_Pointer)
      return T_Pointer
   is
      p : T_Pointer;
      w : T_Word;
   begin
      p := NilPointer;
      w := 0;
      while w <= ObjectTableSize - 2 loop
         p := T_Pointer(w);
         if not getFreeFieldOf(mem, p) then
            exit when fetchClassOf(mem, p) = C_Class;
         end if;
         w := w + 2;
      end loop;
      return p;
   end initialInstanceOf;

   -------------------
   -- instanceAfter --
   -------------------

   function instanceAfter
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return T_Pointer
   is
      p : T_Pointer;
      w : T_Word;
   begin
      p := NilPointer;
      w := T_Word(objectPointer);
      while w <= ObjectTableSize - 2 loop
         p := T_Pointer(w);
         if not getFreeFieldOf(mem, p) then
           exit when fetchClassOf(mem, p) = C_Class;
         end if;
         w := w + 2;
      end loop;
      return p;
   end instanceAfter;

   ------------------
   -- swapPointers --
   ------------------

   procedure swapPointers
     (mem       : in out T_Memory;
      firstPtr  : T_Pointer;
      secondPtr : T_Pointer)
   is
      firstSeg : T_SegmentIndex;
      firstLoc : T_Word;
      firstPnt : Boolean;
      firstOdd : Boolean;
      w : T_Word;
   begin
      firstSeg := getSegmentFieldOf(mem, firstPtr);
      firstLoc := getLocationFieldOf(mem, firstPtr);
      firstPnt := getPtrFieldOf(mem, firstPtr);
      firstOdd := getOddFieldOf(mem, firstPtr);

      w := putSegmentFieldOf(mem, firstPtr, getSegmentFieldOf(mem, secondPtr));
      w := putLocationFieldOf(mem, firstPtr, getLocationFieldOf(mem, secondPtr));
      w := putPtrFieldOf(mem, firstPtr, getPtrFieldOf(mem, secondPtr));
      w := putOddFieldOf(mem, firstPtr, getOddFieldOf(mem, secondPtr));

      w := putSegmentFieldOf(mem, secondPtr, firstSeg);
      w := putLocationFieldOf(mem, secondPtr, firstLoc);
      w := putPtrFieldOf(mem, secondPtr, firstPnt);
      w := putOddFieldOf(mem, secondPtr, firstOdd);
   end swapPointers;

end RSmalltalk.Memory.Objects;
