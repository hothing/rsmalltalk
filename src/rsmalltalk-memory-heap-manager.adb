with RSmalltalk.Memory.ObjTbl; use RSmalltalk.Memory.ObjTbl;

package body RSmalltalk.Memory.Heap.Manager is

   function getHeadOfFreeChunkList
     (mem     : T_Memory;
      size    : T_Word;
      segment : T_SegmentIndex)
      return T_Word
   is
      s : T_Word := size;
   begin
      if s > C_BigSize then s := C_BigSize; end if;
      return get(mem.wordMemory, segment, C_FirstFreeChunkList + s);
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
      s : T_Word := size;
   begin
      if s > C_BigSize then s := C_BigSize; end if;
      put(mem.wordMemory, segment, C_FirstFreeChunkList + s, objectPointer);
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
      ----  | segment |
      ---- segment ~ self segmentBitsOf: objectPointer.
      ---- self classBitsOf: objectPointer put: (self headOfFreeChunkList: size inSegment: segment).
      ---- self headOfFreeChunkList: size inSegment: segment put: objectPointer
      seg := getSegmentFieldOf(mem, objectPointer);
      -- use an field 'Class' as reverse pointer to an object table entry
      -- and the field 'Class' points to the free chunk list of size 'Size'
      putClassFieldOf(mem, objectPointer, getHeadOfFreeChunkList(mem, size, seg));
      -- now we can return the chunk to the free chunklist
      putHeadOfFreeChunkList(mem, size, seg, objectPointer);
   end addToFreeChunkList;

   -----------------------------
   -- removeFromFreeChunkList --
   -----------------------------

   function removeFromFreeChunkList (mem : in out T_Memory;
                                     size : T_Word) return T_Pointer
   is
      op : T_Pointer;
      w : T_Word;
   begin
      --  | objectPointer secondChunk |
      -- objectPointer ~ self headOfFreeChunkList: size inSegment: currentSegment.
      -- objectPointer = NonPointer ifTrue [^nil].
      -- secondChunk ~ self classBitsOf: objectPointer.
      -- self headOfFreeChunkList: size inSegment: currentSegment put: secondChunk.
      -- ^objectPointer
      op := getHeadOfFreeChunkList(mem, size, getCurrentSegment(mem));
      if op /= C_NonPointer then
         -- get the reverse pointer to the free chunk
         w := getClassFieldOf(mem, op);
         putHeadOfFreeChunkList(mem, size, getCurrentSegment(mem), w);
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

   ------------------------------
   ------------------------------

   procedure compactCurrentSegment(mem : in out T_Memory)
   is
   begin
      pragma Compile_Time_Warning(True, "'compactCurrentSegment' unimplemented");
      null;
   end compactCurrentSegment;

   function attemptToAllocateChunkInCurrentSegment(mem : in out T_Memory;
                                   size : T_Word
                                  ) return T_Pointer
   is
      objectPointer, newPointer, predecessor, next : T_Pointer;
      availableSize, excessSize : T_Word;
      seg : T_SegmentIndex;
   begin
      if size < C_BigSize then --- small chunk of exact size handy so use it
         objectPointer := removeFromFreeChunkList(mem, size);
      end if;

      if objectPointer = C_NilPointer then
         predecessor := C_NonPointer;
         --- remember predecessor of chunk under consideration
         seg := getCurrentSegment(mem);
         objectPointer := getHeadOfFreeChunkList(mem, C_BigSize, seg);
         --- the search loop stops when the end of the linked list is encountered
         while objectPointer /= C_NonPointer loop
            availableSize := getSizeFieldOf(mem, objectPointer);
            if availableSize = size then
               -- exactly fitted
               -- remove from free chunk list and return
               next := getClassFieldOf(mem, objectPointer);
               -- the link to the next chunk
               if predecessor = C_NonPointer then
                  putHeadOfFreeChunkList(mem, C_BigSize, seg, next);
               else
                  -- it was between two chunks; link them together
                  putClassFieldOf(mem, predecessor, next);
                  exit;
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
                  exit;
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

   function attemptToAllocateChunk
     (mem  : in out T_Memory;
      size : T_Word)
      return T_Pointer
   is
      op : T_Pointer;
   begin
      op := attemptToAllocateChunkInCurrentSegment(mem, size);
      if op = C_NilPointer then
         for i in 1 .. C_HeapSegmentCount loop
            if getCurrentSegment(mem) > C_LastHeapSegment - 1 then
               putCurrentSegment(mem, C_FirstHeapSegment);
            else
               putCurrentSegment(mem, getCurrentSegment(mem) + 1);
            end if;
            compactCurrentSegment(mem);
            op := attemptToAllocateChunkInCurrentSegment(mem, size);
            exit when op /= C_NilPointer;
         end loop;
      end if;
      return op;
   end attemptToAllocateChunk;


   procedure reclaimInaccessibleObjects(mem : in out T_Memory)
   is
   begin
      pragma Compile_Time_Warning(True, "'reclaimInaccessibleObjects' unimplemented");
      zeroReferenceCounts(mem);
      null;
   end reclaimInaccessibleObjects;


--  allocateChunk: size
--  | objectPointer |
--  objectPointer <- self attemptToAllocateChunk: size.
--  objectPointer isNil ifFalse: [^ objectPointer].
--  self reclaimlnaccessibleObjects.
--  "garbage collect and try again"
--  objectPointer <- self attemptToAllocateChunk: size.
--  objectPointer isNil ifFalse: [^ objectPointer].
--  self outOfMemoryError give up"

   function allocateChunk(mem : in out T_Memory;
                          size : T_Word
                         ) return T_Pointer
   is
      objectPointer : T_Pointer;
   begin
      objectPointer := attemptToAllocateChunk(mem, size);
      if objectPointer /= C_NilPointer then
         return objectPointer;
      end if;
      reclaimInaccessibleObjects(mem);
      objectPointer := attemptToAllocateChunk(mem, size);
      if objectPointer /= C_NilPointer then
         return objectPointer;
      end if;
      mem.errorProc(CE_OutOfMemory);
      return C_NilPointer;
   end allocateChunk;

end RSmalltalk.Memory.Heap.Manager;
