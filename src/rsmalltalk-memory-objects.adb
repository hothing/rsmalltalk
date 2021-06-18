with Ada.Unchecked_Conversion;

package body RSmalltalk.Memory.Objects is

   type T_ValuePredicat is access function(mem : in out T_Memory; value: in T_Word) return Boolean;
   type T_ValueAction is access procedure(mem : in out T_Memory; value: in out T_Word);



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

   --------------------
   -- getSizeFieldOf --
   --------------------

   function getSizeFieldOf
     (mem           : T_Memory;
      objectAddress : T_Word)
      return T_Word
   is
   begin
      -- ^self heapChunkOf: objectPointer word: 0
      return getHeapChunkOf(mem, objectAddress, 0);
   end getSizeFieldOf;

   --------------------
   -- putSizeFieldOf --
   --------------------

   procedure putSizeFieldOf
     (mem           : in out T_Memory;
      objectAddress : T_Word;
      value         : T_Word)
   is
   begin
      putHeapChunkOf(mem, objectAddress, 0, value);
   end putSizeFieldOf;

   ---------------------
   -- getClassFieldOf --
   ---------------------

   function getClassFieldOf
     (mem           : T_Memory;
      objectAddress : T_Word)
      return T_Word
   is
   begin
      -- ^self heapChunkOf: objectPointer word: 1
      return getHeapChunkOf(mem, objectAddress, 1);
   end getClassFieldOf;

   ---------------------
   -- putClassFieldOf --
   ---------------------

   procedure putClassFieldOf
     (mem           : in out T_Memory;
      objectAddress : T_Word;
      value         : T_Word)
   is
   begin
      putHeapChunkOf(mem, objectAddress, 1, value);
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
            methodHdr := getHeapChunkOf(mem, objectPointer, C_HeaderSize);
            return C_HeaderSize + 1 + (methodHdr and 126) / 2; -- TOCHECK: might be wrong
         else
            return getSizeFieldOf(mem, objectPointer);
         end if;
      else
         return C_HeaderSize;
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
      if (size < C_HugeSize) or not getPtrFieldOf(mem, objectPointer) then
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
      -- self headOfFreePointerListPut: (self IocationBitsOf: objectPointer).
      -- ^objectPointer
      op := C_NilPointer;
      op := getHeadOfFreePointerList(mem);
      if op /= C_NonPointer then
         putHeadOfFreePointerList(mem, getLocationFieldOf(mem, op));
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
      return get(mem.wordMemory, segment, C_FirstFreeChunkList + size);
   end getHeadOfFreeChunkList;

   ----------------------------
   -- putHeadOfFreeChunkList --
   ----------------------------

   procedure putHeadOfFreeChunkList
     (mem           : in out T_Memory;
      size          : T_Word;
      segment       : T_SegmentIndex;
      objectPointer : T_Pointer)
   is
   begin
      -- ^wordMemory segment: segment word: FirstFreeChunkList + size put: objectPointer
      put(mem.wordMemory, segment, C_FirstFreeChunkList + size, objectPointer);
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
   begin
      --  | segment |
      -- segment ~ self segmentBitsOf: objectPointer.
      -- self classBitsOf: objectPointer put: (self headOfFreeChunkList: size inSegment: segment).
      -- self headOfFreeChunkList: size inSegment: segment put: objectPointer
      seg := getSegmentFieldOf(mem, objectPointer);
      putClassFieldOf(mem, objectPointer, getHeadOfFreeChunkList(mem, size, seg));
      putHeadOfFreeChunkList(mem, size, seg, objectPointer);
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
      if op /= C_NonPointer then
         w := getClassFieldOf(mem, op);
         putHeadOfFreeChunkList(mem, size, mem.currentSegment, w);
      else
         op := C_NilPointer;
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
   begin
      --  self headOfFreeChunkList: size inSegment: segment put: NonPointer
      putHeadOfFreeChunkList(mem, size, segment, C_NonPointer);
   end resetFreeChunkList;

   --------------
   -- allocate --
   --------------

   function allocate (
                      mem : in out T_Memory;
                      size : T_Word;
                      classPointer : T_Pointer) return T_Pointer is
      op : T_Pointer;
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
      putClassFieldOf(mem, op, classPointer);
      for i in C_HeaderSize .. (size - 1) loop
         putHeapChunkOf(mem, op, i, C_NilPointer);
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
      if op = C_NilPointer then
         -- garbage collect and try again
         reclaimInaccessibleObjects(mem);
         op := attemptToAllocateChunk(mem, size);
         if op = C_NilPointer then mem.errorProc(CE_OutOfMemory); end if;
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
        for i in 1 .. C_HeapSegmentCount loop
           -- first attempt to get a chunk
           op := attemptToAllocateChunkInCurrentSegment(mem, size);
           exit when op /= C_NilPointer;
           -- try to extract a space in current segment
           compactCurrentSegment(mem);
           -- second attempt to get a chunk
           -- after compaction
           op := attemptToAllocateChunkInCurrentSegment(mem, size);
           exit when op /= C_NilPointer;
           -- no, there is no space
           -- try to use another segment
           mem.currentSegment := mem.currentSegment + 1;
           if mem.currentSegment > C_LastHeapSegment then
              mem.currentSegment := C_FirstHeapSegment;
           end if;
        end loop;
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
      availableSize, excessSize : T_Word;
   begin
      if size < C_BigSize then --- small chunk of exact size handy so use it
         objectPointer := removeFromFreeChunkList(mem, size);
      end if;

      if objectPointer = C_NilPointer then
         predecessor := C_NonPointer;
         --- remember predecessor of chunk under consideration
         objectPointer := getHeadOfFreeChunkList(mem, C_BigSize, mem.currentSegment);
         --- the search loop stops when the end of the linked list is encountered
         while objectPointer /= C_NonPointer loop
            availableSize := getSizeFieldOf(mem, objectPointer);
            if availableSize = size then --- exact fit --remove from free chunk list and return
               next := getClassFieldOf(mem, objectPointer); --- the link to the next chunk
               if predecessor = C_NonPointer then
                  putHeadOfFreeChunkList(mem, C_BigSize, mem.currentSegment, next);
               else --- it was between two chunks; link them together
                  putClassFieldOf(mem, predecessor, next);
                  exit; --!! TO REWORK
               end if;
            else
               excessSize := availableSize - size;
               if excessSize >= C_HeaderSize then
                  --- can be broken into two usable parts: return the second part
                  --- obtain an object table entry for the second part
                  newPointer := obtainPointer(mem, size, getLocationFieldOf(mem, objectPointer) + excessSize);
                  if newPointer /= C_NilPointer then
                     --- correct the size of the first part (which remains on the free list)
                     putSizeFieldOf(mem, objectPointer, excessSize);
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
   begin
      objectPointer := removeFromFreeChunkList(mem, size);
      if objectPointer /= C_NilPointer then
         putObjectRef(mem, objectPointer, 0);
         putSegmentFieldOf(mem, objectPointer, mem.currentSegment);
         putLocationFieldOf(mem, objectPointer, location);
         putSizeFieldOf(mem, objectPointer, size);
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
      addToFreeChunkList(mem, T_Word'Min(space, C_BigSize), classPointer);
   end deallocate;

   --------------------------------
   -- abandonFreeChunkslnSegment --
   --------------------------------

   function abandonFreeChunkslnSegment
     (mem     : in out T_Memory;
      segment : T_SegmentIndex)
      return T_Word
   is
      lowWaterMark : T_Word;
      objectPointer, nextPointer : T_Pointer;
   begin
      lowWaterMark := C_HeapSpaceStop; --- first assume that no chunk is free
      declare
         subtype T_Counter is T_Word range C_HeaderSize .. C_BigSize;
      begin
         for size in T_Counter loop --- for each free-chunk list
            objectPointer := getHeadOfFreeChunkList(mem, size, segment);
            while objectPointer /= C_NonPointer loop
               lowWaterMark := T_Word'Min(lowWaterMark, getLocationFieldOf(mem, objectPointer));
               nextPointer := getClassFieldOf(mem, objectPointer);
               --- link to next free chunk
               putClassFieldOf(mem, objectPointer, C_NonPointer);
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
   begin
      putFreeFieldOf(mem, ptr, True);
      addToFreePointerList(mem, ptr);
   end releasePointer;

   ------------------------------
   -- reverseHeapPointersAbove --
   ------------------------------

   procedure reverseHeapPointersAbove (mem : in out T_Memory; lowWaterMark : T_Word) is
      size : T_Word;
      objectPointer : T_Pointer;
   begin
      objectPointer := 0;
      while objectPointer < (C_ObjectTableSize - 2) loop
         if not getFreeFieldOf(mem, objectPointer) then
            --- the Object Table entry is in use
            if getSegmentFieldOf(mem, objectPointer) = mem.currentSegment then
               --- the object is in this segment
               if getLocationFieldOf(mem, objectPointer) >= lowWaterMark then
                  --- the object will be swept
                  size := getSizeFieldOf(mem, objectPointer); --- rescue the size
                  putSizeFieldOf(mem, objectPointer, objectPointer); --- reverse the pointer
                  putLocationFieldOf(mem, objectPointer, size); --- save the size
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
      while si < C_HeapSpaceStop loop --- for each object, si
         if get(mem.wordMemory, mem.currentSegment, si + 1) = C_NonPointer then
            --- unallocated, so skip it
            size := get(mem.wordMemory, mem.currentSegment, si);
            si := si + size;
         else
            --- allocated, so keep it, but move it to compact storage
            objectPointer := get(mem.wordMemory, mem.currentSegment, si);
            size := getLocationFieldOf(mem, objectPointer);
            --- the reversed size
            putLocationFieldOf(mem, objectPointer, di); --- point object table at new location
            putSizeFieldOf(mem, objectPointer, size); --- restore the size to its proper place
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
      if lowWaterMark < C_HeapSpaceStop then
         reverseHeapPointersAbove(mem, lowWaterMark);
         bigSpace := sweepCurrentSegmentFrom(mem, lowWaterMark);
         deallocate(mem, obtainPointer(mem, C_HeapSpaceStop + 1 - bigSpace, bigSpace));
      end if;
   end compactCurrentSegment;

   ---------------------------
   --*  Garbage collection *--
   ---------------------------

   procedure forAllOtherObjectsAccessibleFrom(mem : in out T_Memory;
                                             objectPointer: in out T_Pointer;
                                             predicate : T_ValuePredicat;
                                             action : T_ValueAction
                                            )
   is
      w, offset : T_Word;
      next : T_Pointer;
   begin
      w := getLastPointerOf(mem, objectPointer) - 1;
      offset := 0;
      while offset < w loop
         next := getHeapChunkOf(mem, objectPointer, offset);
         if not isIntegerObject(next) and predicate(mem, next) then
            --- it's a non-immediate object and it should be processed
            forAllOtherObjectsAccessibleFrom(mem, next, predicate, action);
         end if;
         offset := offset + 1;
      end loop;
      --- all pointers have been followed; now perform the action
      action(mem, objectPointer);
   end forAllOtherObjectsAccessibleFrom;

   procedure forAllObjectsAccessibleFrom(mem : in out T_Memory;
                                        objectPointer: in out T_Pointer;
                                        predicate : T_ValuePredicat;
                                        action : T_ValueAction
                                       )
   is
      p : T_Pointer;
   begin
      p := objectPointer;
      if not isIntegerObject(p) and predicate(mem, p) then
         --- it's a non-immediate object and it should be processed
         forAllOtherObjectsAccessibleFrom(mem, p, predicate, action);
      end if;
   end forAllObjectsAccessibleFrom;

   -------------
   -- countUp --
   -------------

   procedure countUp
     (mem : in out T_Memory;
      objectPointer : T_Pointer)
   is
      count : T_Word;
   begin
      if not isIntegerObject(objectPointer) then
         count := getCountFieldOf(mem, objectPointer);
         if count <= 128 then
            putCountFieldOf(mem, objectPointer, count + 1);
         end if;
      end if;
   end countUp;

   ---------------
   -- countDown --
   ---------------

   function countDown_predicat(mem : in out T_Memory; value: in T_Word) return Boolean is
      count : T_Word;
   begin
      count := getCountFieldOf(mem, value) - 1;
      if count < 127 then
         putCountFieldOf(mem, value, count);
      end if;
      return count = 0;
   end countDown_predicat;

   procedure countDown_action(mem : in out T_Memory; value: in out T_Word) is
   begin
      putCountFieldOf(mem, T_Pointer(value), 0);
      deallocate(mem, T_Pointer(value));
   end countDown_action;

   procedure countDown
     (mem : in out T_Memory;
      objectPointer : T_Pointer)
   is
      p : T_Pointer := objectPointer;
   begin
      --pragma Compile_Time_Warning(True, "[countDown] is not implemented");
      if not isIntegerObject(p) then
         --- the predicate decrements the count and tests for zero
         --- the action zeroes the count and deallocates the object
         forAllObjectsAccessibleFrom(mem, p, countDown_predicat'Access, countDown_action'Access);
      end if;
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
      offset : T_Word;
   begin
      offset := 0;
      while offset < C_ObjectTableSize - 2 loop
         putCountFieldOf(mem, offset, 0);
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
   begin
      putCountFieldOf(mem, value, 1);
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
      p, p2, p3 : T_Pointer;
   begin
      --- reset heads of free-chunk lists
      seg := C_FirstHeapSegment;
      while seg < C_LastHeapSegment loop --- for every segment
         s := C_HeaderSize;
         while s < C_BigSize loop
            --- reset the list head
            resetFreeChunkList(mem, s, seg);
            s := s + 1;
         end loop;
         seg := seg + 1;
      end loop;
      --- rectify counts, and deallocate garbage
      p := 0;
      while p < C_ObjectTableSize - 2 loop --- for every object table entry
         if not getFreeFieldOf(mem, p) then --- if it is not a free entry
            count := getCountFieldOf(mem, p);
            if count = 0 then --- it is unmarked, so deallocate it
               deallocate(mem, p);
            else --- it is marked, so rectify reference counts
               if count < 128 then ---  subtract 1 to compensate for the mark
                  putCountFieldOf(mem, p, count - 1);
               end if;
               p2 := 1;
               p3 := getLastPointerOf(mem, p) - 1;
               while p2 <= p3 loop --- increment the reference count of each pointer
                  countUp(mem, getHeapChunkOf(mem, p, p2));
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
   begin
      countUp(mem, classPointer);
      op := allocateChunk(mem, size + (extraWord mod 2));
      putClassFieldOf(mem, op, classPointer);
      putOddFieldOf(mem, op, odd);
      putPtrFieldOf(mem, op, ptr);
      default := C_NilPointer;
      if (not ptr) then default := 0; end if;
      for i in C_HeaderSize .. size - 1 loop
         putHeapChunkOf(mem, op, T_Word(i), default);
      end loop;
      putSizeFieldOf(mem, op, size);
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
      return getHeapChunkOf(mem, objectPointer, C_HeaderSize + fieldIndex);
   end fetchPointer;

   ------------------
   -- storePointer --
   ------------------

   procedure storePointer
     (mem           : in out T_Memory;
      fieldIndex    : T_Word;
      objectPointer : T_Pointer;
      valuePointer  : T_Word)
   is
      chnk : T_Word;
   begin
      chnk := C_HeaderSize + fieldIndex;
      countUp(mem, valuePointer);
      countDown(mem, getHeapChunkOf(mem, objectPointer, chnk));
      putHeapChunkOf(mem, objectPointer, chnk, valuePointer);
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
      return getHeapChunkOf(mem, objectPointer, C_HeaderSize + wordIndex);
   end fetchWord;

   ---------------
   -- storeWord --
   ---------------

   procedure storeWord
     (mem           : in out T_Memory;
      wordIndex     : T_Word;
      objectPointer : T_Pointer;
      valueWord     : T_Word)
   is
   begin
      putHeapChunkOf(mem, objectPointer, C_HeaderSize, valueWord);
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
      return getHeapChunkByteOf(mem, objectPointer, C_HeaderSize * 2 + byteIndex);
   end fetchByte;

   ---------------
   -- storeByte --
   ---------------

   procedure storeByte
     (mem           : in out T_Memory;
      byteIndex     : T_Word;
      objectPointer : T_Pointer;
      valueByte     : T_Byte)
   is
   begin
      putHeapChunkByteOf(mem, objectPointer, C_HeaderSize * 2 + byteIndex, valueByte);
   end storeByte;

   --------------------------
   -- increaseReferencesTo --
   --------------------------

   procedure increaseReferencesTo (mem : in out T_Memory; objectPointer : T_Pointer) is
   begin
      countUp(mem, objectPointer);
   end increaseReferencesTo;

   --------------------------
   -- decreaseReferencesTo --
   --------------------------

   procedure decreaseReferencesTo (mem : in out T_Memory; objectPointer : T_Pointer) is
   begin
      countDown(mem, objectPointer);
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
      return getSizeFieldOf(mem, objectPointer) - C_HeaderSize;
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
      size := C_HeaderSize + length;
      if size < C_HugeSize then extra := 0;
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
      size := C_HeaderSize + length;
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
      size := C_HeaderSize + (length + 1) / 2;
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
      p := C_NilPointer;
      w := 0;
      while w <= C_ObjectTableSize - 2 loop
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
      p := C_NilPointer;
      w := T_Word(objectPointer);
      while w <= C_ObjectTableSize - 2 loop
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
   begin
      firstSeg := getSegmentFieldOf(mem, firstPtr);
      firstLoc := getLocationFieldOf(mem, firstPtr);
      firstPnt := getPtrFieldOf(mem, firstPtr);
      firstOdd := getOddFieldOf(mem, firstPtr);

      putSegmentFieldOf(mem, firstPtr, getSegmentFieldOf(mem, secondPtr));
      putLocationFieldOf(mem, firstPtr, getLocationFieldOf(mem, secondPtr));
      putPtrFieldOf(mem, firstPtr, getPtrFieldOf(mem, secondPtr));
      putOddFieldOf(mem, firstPtr, getOddFieldOf(mem, secondPtr));

      putSegmentFieldOf(mem, secondPtr, firstSeg);
      putLocationFieldOf(mem, secondPtr, firstLoc);
      putPtrFieldOf(mem, secondPtr, firstPnt);
      putOddFieldOf(mem, secondPtr, firstOdd);
   end swapPointers;

end RSmalltalk.Memory.Objects;
