with RSmalltalk.Memory.Segmented; use RSmalltalk.Memory.Segmented;

package RSmalltalk.Memory.Objects is

   type T_Memory (scount : T_SegmentIndex) is private;

   subtype T_Counter_5 is T_Byte range 0 .. (2**5 - 1);
   subtype T_Counter_6 is T_Byte range 0 .. (2**5 - 1);
   subtype T_Counter_8 is T_Byte;
   subtype T_Counter_11 is T_Word range 0 .. (2**11 - 1);
   
   type T_ObjectEntryHeader is record
      count   : T_Counter_8;
      p       : Boolean;
      o       : Boolean;
      f       : Boolean;
      segment : T_SegmentIndex;
   end record;
   for T_ObjectEntryHeader use record
      segment at 0 range 0 ..  3; -- see HeapSegmentCount
      p       at 0 range 4 ..  4;
      o       at 0 range 5 ..  5;
      f       at 0 range 6 ..  6;
      count   at 0 range 8 .. 15;
   end record;
   for T_ObjectEntryHeader'Size use T_Word'Size;

--     type T_ObjectTableEntry is record
--        hdr      : T_ObjectEntryHeader;
--        location : T_Pointer;
--     end record;

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
   
   ------------------

   type T_ErrorHandler is access procedure(errId : T_Word);

   procedure cantBeIntegerObject (mem : T_Memory; 
                                  objectPointer : T_Pointer);

   function getObjectRef (mem : T_Memory; objectPointer : T_Pointer) return T_Word;

   function putObjectRef
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word) return T_Word;

   function getCountFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word;

   function putCountFieldOf
     (mem           : in out T_Memory;
      objectPointer : in T_Pointer;
      value         : in T_Word) return T_Word;

   function getOddFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return Boolean;

   function putOddFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : Boolean)
      return T_Word;

   function getFreeFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return Boolean;

   function putFreeFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : Boolean)
      return T_Word;

   function getPtrFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return Boolean;

   function putPtrFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : Boolean)
      return T_Word;

   function getSegmentFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return T_SegmentIndex;

   function putSegmentFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_SegmentIndex)
      return T_Word;

   function getLocationFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word;

   function putLocationFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word) return T_Word;

   -- ObjectMemory:HeapChunk
   function getHeapChunkOf
     (mem           : T_Memory;
      objectPointer : T_Pointer;
      offset        : T_Word) return T_Word;

   function putHeapChunkOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      offset        : T_Word;
      value         : T_Word)
      return T_Word;

   function getHeapChunkByteOf
     (mem           : T_Memory;
      objectPointer : T_Pointer;
      offset        : T_Word) return T_Byte;

   function putHeapChunkByteOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      offset        : T_Word;
      value         : T_Byte)
      return T_Byte;

   --
   function getSizeFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word;

   function putSizeFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
      return T_Word;

   function getClassFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word;

   function putClassFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word)
      return T_Word;

   function getLastPointerOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word;

   function getSpaceOccupiedBy
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word;

   -- ObjectMemory methods for word memory
   function getHeadOfFreePointerList (mem : T_Memory) return T_Word;

   function putHeadOfFreePointerList
     (mem           : in out T_Memory;
      objectPointer : T_Pointer)
      return T_Word;

   procedure addToFreePointerList (mem : in out T_Memory;
                                   objectPointer : T_Pointer);

   function removeFromFreePointerList (mem : in out T_Memory) return T_Pointer;

   function getHeadOfFreeChunkList
     (mem     : T_Memory;
      size    : T_Word;
      segment : T_SegmentIndex) return T_Word;

   function putHeadOfFreeChunkList
     (mem           : in out T_Memory;
      size          : T_Word;
      segment       : T_SegmentIndex;
      objectPointer : T_Pointer)
      return T_Word;

   procedure addToFreeChunkList
     (mem           : in out T_Memory;
      size          : T_Word;
      objectPointer : T_Pointer);

   function removeFromFreeChunkList (mem : in out T_Memory;
                                     size : T_Word) return T_Pointer;

   procedure resetFreeChunkList
     (mem     : in out T_Memory;
      size    : T_Word;
      segment : T_SegmentIndex);

   -- ObjectMemory methods for Allocation/Deallocation
   function allocate
     (mem          : in out T_Memory;
      size         : T_Word;
      odd          : Boolean;
      ptr          : Boolean;
      extraWord    : T_Word;
      classPointer : T_Pointer)
      return T_Pointer;

   function allocateChunk (mem : in out T_Memory;
                           size : T_Word) return T_Pointer;

   function attemptToAllocateChunk
     (mem  : in out T_Memory;
      size : T_Word)
      return T_Pointer;

   function attemptToAllocateChunkInCurrentSegment
     (mem  : in out T_Memory;
      size : T_Word)
      return T_Pointer;

   function obtainPointer
     (mem       : in out T_Memory;
      size      : T_Word;
      location : T_Word)
      return T_Pointer;

   procedure deallocate (mem : in out T_Memory;
                         classPointer : T_Pointer);

   -- ObjectMemory:Compaction
   function abandonFreeChunkslnSegment
     (mem     : in out T_Memory;
      segment : T_SegmentIndex)
      return T_Word;

   procedure releasePointer (mem : in out T_Memory; ptr : T_Pointer);

   procedure reverseHeapPointersAbove (mem : in out T_Memory;
                                       lowWaterMark : T_Word);

   function sweepCurrentSegmentFrom (mem : in out T_Memory;
                                     lowWaterMark : T_Word)
                                     return T_Word;

   procedure compactCurrentSegment (mem : in out T_Memory);

   -- ObjectMemory:GarbageCollectioon
   -- forAllObjectsAccessibleFrom: objectPointer suchThat: predicate do: action
   -- forAllOtherObjectsAccessibleFrom: objectPointer suchThat: predicate do: action

--   function forAllObjectsAccessibleFrom (mem : in out T_Memory;
--                                         objectPointer: in out T_Pointer;
--                                         predicate : T_Predicat;
--                                         action : T_Action) return T_Pointer;

   function countUp (mem : in out T_Memory;
                     objectPointer : T_Pointer)
                     return T_Pointer;

   function countDown (mem : in out T_Memory;
                       objectPointer : T_Pointer)
                       return T_Pointer;

   --function getRootObjects(mem : T_Memory) return T_WordsSeq_Ptr;
   --function getRootObjectsList(mem : T_Memory) return T_WordsList_Ptr;

   procedure reclaimInaccessibleObjects (mem : in out T_Memory);

   procedure zeroReferenceCounts (mem : in out T_Memory);

   procedure markAccessibleObjects (mem : in out T_Memory);

   function markObjectsAccessibleFrom (mem: in out T_Memory;
                                       objectPointer : in out T_Pointer)
                                       return T_Pointer;

   procedure rectifyCountsAndDeallocateGarbage (mem : in out T_Memory);

   -- ObjectMemory methods for Interpreter
   function fetchPointer
     (mem           : T_Memory;
      fieldIndex    : T_Word;
      objectPointer : T_Pointer) return T_Pointer;

   function storePointer
     (mem           : in out T_Memory;
      fieldIndex    : T_Word;
      objectPointer : T_Pointer;
      valuePointer  : T_Word) return T_Word;

   function fetchWord
     (mem           : T_Memory;
      wordIndex     : T_Word;
      objectPointer : T_Pointer) return T_Word;

   function storeWord
     (mem           : in out T_Memory;
      wordIndex     : T_Word;
      objectPointer : T_Pointer;
      valueWord     : T_Word) return T_Word;

   function fetchByte
     (mem           : T_Memory;
      byteIndex     : T_Word;
      objectPointer : T_Pointer) return T_Byte;

   function storeByte
     (mem           : in out T_Memory;
      byteIndex     : T_Word;
      objectPointer : T_Pointer;
      valueByte     : T_Byte) return T_Byte;

   procedure increaseReferencesTo (mem : in out T_Memory;
                                   objectPointer : T_Pointer);

   procedure decreaseReferencesTo (mem : in out T_Memory;
                                   objectPointer : T_Pointer);

   function fetchClassOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Pointer;

   function fetchWordLengthOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word;

   function fetchByteLengthOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word;

   function instantiateClassWithPointers
     (mem          : in out T_Memory;
      classPointer : T_Pointer;
      length       : T_Word)
      return T_Pointer;

   function instantiateClassWithWords
     (mem          : in out T_Memory;
      classPointer : T_Pointer;
      length       : T_Word)
      return T_Pointer;

   function instantiateClassWithBytes
     (mem          : in out T_Memory;
      classPointer : T_Pointer;
      length       : T_Word)
      return T_Pointer;

   function initialInstanceOf
     (mem          : in out T_Memory;
      classPointer : T_Pointer)
      return T_Pointer;

   function instanceAfter
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Pointer;

   procedure swapPointers
     (mem       : in out T_Memory;
      firstPtr  : T_Pointer;
      secondPtr : T_Pointer);

private
   
    type T_Memory (scount : T_SegmentIndex) is record -- this is RealObjectMemory
      wordMemory     : T_SegmentedMemory (scount);
      currentSegment : T_SegmentIndex;
      errorProc      : T_ErrorHandler;
   end record;
 
end RSmalltalk.Memory.Objects;
