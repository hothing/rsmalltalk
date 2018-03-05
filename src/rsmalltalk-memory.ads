with Interfaces; use Interfaces;

package RSmalltalk.Memory is

   subtype Byte is Unsigned_8;
   subtype Word is Unsigned_16;
   subtype SmallInt is Integer_16;

   subtype Pointer is Unsigned_16;

   -- subtype WordIndex is Word range 0 .. 2**15; -- index of word in memory

   -- Heap constants ^ declarations
   HeapSegmentCount : constant := 16;
   FirstHeapSegment : constant := 1;
   LastHeapSegment  : constant := (FirstHeapSegment + HeapSegmentCount - 1);
   HeaderSize       : constant := 2;
   HeapSpaceStop    : constant := 0;

   subtype T_Segment is Byte range 0 .. (HeapSegmentCount - 1);

   type T_DataForm is (tWord16, tBytes2, tBits16);
   type T_WordBytes is array (0 .. 1) of Byte;
   type T_WordBools is array (0 .. 15) of Boolean;
   type T_Words2 is array (0 .. 1) of Word;

   type T_Word16 (d : T_DataForm := tWord16) is record
      case d is
         when tWord16 =>
            x : Word;
         when tBytes2 =>
            xb : T_WordBytes;
         when tBits16 =>
            bm : T_WordBools;
      end case;
   end record;
   pragma Unchecked_Union (T_Word16);

   type MemorySegment is array (Word) of T_Word16;
   type MemorySegments is array (T_Segment range <>) of MemorySegment;

   type RealWordMemory (len : T_Segment) is record
      segments : MemorySegments (0 .. len);
   end record;

   -- Object Table constants ^ declarations
   ObjectTableSegment : constant := 0;
   ObjectTableStart   : constant := 0;
   ObjectTableSize    : constant := 65535;

   BigSize            : constant := 20;
   HugeSize           : constant := 256;

   NilPointer         : constant := 0;
   NonPointer         : constant := 65535;

   FreePointerList    : constant := 0;
   FirstFreeChunkList : constant := 0;
   LastFreeChunkList  : constant := FirstFreeChunkList + BigSize;



   subtype T_Counter is Byte;
   type ObjectEntryHeader is record
      count   : T_Counter;
      p       : Boolean;
      o       : Boolean;
      f       : Boolean;
      segment : T_Segment;
   end record;

   for ObjectEntryHeader use record
      segment at 0 range 0 ..  3;
      p       at 0 range 4 ..  4;
      o       at 0 range 5 ..  5;
      f       at 0 range 6 ..  6;
      count   at 0 range 8 .. 15;
   end record;
   for ObjectEntryHeader'Size use Word'Size;

   type ObjectTableEntry is record
      hdr      : ObjectEntryHeader;
      location : Word;
   end record;

   type T_WordToObjectEntry (d : Boolean := False) is record
      case d is
         when False =>
            x : ObjectEntryHeader;
         when True =>
            xb : Word;
      end case;
   end record;
   pragma Unchecked_Union (T_WordToObjectEntry);
   for T_WordToObjectEntry'Size use Word'Size;

   subtype T_SmallInteger is Integer_16 range 0 .. 2**14;
   type T_Value is record
      value : T_SmallInteger;
      smi   : Boolean;
   end record;

   for T_Value use record
      smi   at 0 range 0 ..  0;
      value at 0 range 1 .. 15;
   end record;
   for T_Value'Size use Word'Size;

   type T_ValuePointer (d : Boolean := False) is record
      case d is
         when False =>
            x : Word;
         when True =>
            xb : T_Value;
      end case;
   end record;
   pragma Unchecked_Union (T_ValuePointer);
   for T_ValuePointer'Size use Word'Size;

   type T_HookProcError is access procedure(errId : Word);

   CE_NoError      : constant := 0;
   CE_NotObject    : constant := 1;
   CE_OutOfMemory  : constant := 2;

   type STMemory (scount : T_Segment) is record -- this is RealObjectMemory
      wordMemory     : RealWordMemory (scount);
      currentSegment : T_Segment;
      res            : Word;
      errorProc      : T_HookProcError;
   end record;

   type T_Predicat is access function(mem : in out STMemory; value: Word) return Boolean;
   type T_Action is access procedure(mem : in out STMemory; value: in out Word);

   type T_WordsList;
   type T_WordsList_Ptr is access all T_WordsList;
   type T_WordsList is record
      val : Word;
      next : T_WordsList_Ptr;
   end record;

   type T_WordsSeq is array (Word range <>) of Word;
   type T_WordsSeq_Ptr is access all T_WordsSeq;

   C_Class        : constant := 1;
   C_IntegerClass : constant := 2;
   C_MethodClass  : constant := 20;

   function getWord
     (rwm     : in RealWordMemory;
      segment :    T_Segment;
      index   :    Word) return Word;
   function getByte
     (rwm       : in RealWordMemory;
      segment   :    T_Segment;
      index     :    Word;
      byteIndex :    Byte) return Byte;
   function putWord
     (rwm     : in out RealWordMemory;
      segment :        T_Segment;
      index   :        Word;
      value   :        Word) return Word;
   function putByte
     (rwm       : in out RealWordMemory;
      segment   :        T_Segment;
      index     :        Word;
      byteIndex :        Byte;
      value     :        Byte) return Byte;

   -- ObjectMemory:ObjectTable
   function integerValueOf (ptr : Pointer) return SmallInt;
   function integerObjectOf (value : SmallInt) return Pointer;
   function isIntegerValue (value : SmallInt) return Boolean;
   procedure cantBeIntegerObject (mem : STMemory; objectPointer : Pointer);
   function isIntegerObject
     ( mem: STMemory;
       objectPointer : Pointer)  return Boolean;

   function getObjectRef (mem : STMemory; objectPointer : Pointer) return Word;
   function putObjectRef
     (mem           : in out STMemory;
      objectPointer : Pointer;
      value         : Word) return Word;

   function getCountFieldOf
     (mem           : STMemory;
      objectPointer : Pointer) return Word;
   function putCountFieldOf
     (mem           : in out STMemory;
      objectPointer : Pointer;
      value         : Word) return Word;

   function getOddFieldOf
     (mem           : STMemory;
      objectPointer : Pointer) return Boolean;
   function putOddFieldOf
     (mem           : in out STMemory;
      objectPointer : Pointer;
      value         : Boolean) return Boolean;

   function getFreeFieldOf
     (mem           : STMemory;
      objectPointer : Pointer) return Boolean;
   function putFreeFieldOf
     (mem           : in out STMemory;
      objectPointer : Pointer;
      value         : Boolean) return Boolean;

   function getPtrFieldOf
     (mem           : STMemory;
      objectPointer : Pointer) return Boolean;
   function putPtrFieldOf
     (mem           : in out STMemory;
      objectPointer : Pointer;
      value         : Boolean) return Boolean;

   function getSegmentFieldOf
     (mem           : STMemory;
      objectPointer : Pointer) return T_Segment;
   function putSegmentFieldOf
     (mem           : in out STMemory;
      objectPointer : Pointer;
      value         : T_Segment) return T_Segment;

   function getLocationFieldOf
     (mem           : STMemory;
      objectPointer : Pointer) return Word;
   function putLocationFieldOf
     (mem           : in out STMemory;
      objectPointer : Pointer;
      value         : Word) return Word;

   -- ObjectMemory:HeapChunk
   function getHeapChunkOf
     (mem           : STMemory;
      objectPointer : Pointer;
      offset        : Word) return Word;
   function putHeapChunkOf
     (mem           : in out STMemory;
      objectPointer : Pointer;
      offset        : Word;
      value         : Word)
      return Word;

   function getHeapChunkByteOf
     (mem           : STMemory;
      objectPointer : Pointer;
      offset        : Word) return Byte;
   function putHeapChunkByteOf
     (mem           : in out STMemory;
      objectPointer : Pointer;
      offset        : Word;
      value         : Byte)
      return Byte;

   --
   function getSizeFieldOf
     (mem           : STMemory;
      objectPointer : Pointer) return Word;
   function putSizeFieldOf
     (mem           : in out STMemory;
      objectPointer : Pointer;
      value         : Word)
      return Word;

   function getClassFieldOf
     (mem           : STMemory;
      objectPointer : Pointer) return Word;
   function putClassFieldOf
     (mem           : in out STMemory;
      objectPointer : Pointer;
      value         : Word)
      return Word;

   function getLastPointerOf
     (mem           : STMemory;
      objectPointer : Pointer) return Word;
   function getSpaceOccupiedBy
     (mem           : STMemory;
      objectPointer : Pointer) return Word;

   -- ObjectMemory methods for word memory
   function getHeadOfFreePointerList (mem : STMemory) return Word;
   function putHeadOfFreePointerList
     (mem           : in out STMemory;
      objectPointer : Pointer)
      return Word;

   procedure addToFreePointerList (mem : in out STMemory; objectPointer : Pointer);
   function removeFromFreePointerList (mem : in out STMemory) return Pointer;

   function getHeadOfFreeChunkList
     (mem     : STMemory;
      size    : Word;
      segment : T_Segment) return Word;
   function putHeadOfFreeChunkList
     (mem           : in out STMemory;
      size          : Word;
      segment       : T_Segment;
      objectPointer : Pointer)
      return Word;

   procedure addToFreeChunkList
     (mem           : in out STMemory;
      size          : Word;
      objectPointer : Pointer);
   function removeFromFreeChunkList (mem : in out STMemory; size : Word) return Pointer;
   procedure resetFreeChunkList
     (mem     : in out STMemory;
      size    : Word;
      segment : T_Segment);

   -- ObjectMemory methods for Allocation/Deallocation
   function allocate
     (mem          : in out STMemory;
      size         : Word;
      odd          : Boolean;
      ptr          : Boolean;
      extraWord    : Word;
      classPointer : Pointer)
      return Pointer;

   function allocateChunk (mem : in out STMemory; size : Word) return Pointer;
   function attemptToAllocateChunk
     (mem  : in out STMemory;
      size : Word)
      return Pointer;
   function attemptToAllocateChunkInCurrentSegment
     (mem  : in out STMemory;
      size : Word)
      return Pointer;
   function obtainPointer
     (mem       : in out STMemory;
      size      : Word;
      location : Word)
      return Pointer;
   procedure deallocate (mem : in out STMemory; classPointer : Pointer);

   -- ObjectMemory:Compaction
   function abandonFreeChunkslnSegment
     (mem     : in out STMemory;
      segment : T_Segment)
      return Word;
   procedure releasePointer (mem : in out STMemory; ptr : Pointer);
   procedure reverseHeapPointersAbove (mem : in out STMemory; lowWaterMark : Word);
   function sweepCurrentSegmentFrom (mem : in out STMemory; lowWaterMark : Word) return Word;
   procedure compactCurrentSegment (mem : in out STMemory);

   -- ObjectMemory:GarbageCollectioon
   -- forAllObjectsAccessibleFrom: objectPointer suchThat: predicate do: action
   -- forAllOtherObjectsAccessibleFrom: objectPointer suchThat: predicate do: action

   function forAllObjectsAccessibleFrom (mem : in out STMemory; objectPointer: in out Pointer; predicate : T_Predicat; action : T_Action) return Pointer;

   function countUp (mem : in out STMemory;
      objectPointer : Pointer)
                     return Pointer;
   function countDown (mem : in out STMemory;
      objectPointer : Pointer)
                       return Pointer;

   function getRootObjects(mem : STMemory) return T_WordsSeq_Ptr;
   function getRootObjectsList(mem : STMemory) return T_WordsList_Ptr;

   procedure reclaimInaccessibleObjects (mem : in out STMemory);
   procedure zeroReferenceCounts (mem : in out STMemory);
   procedure markAccessibleObjects (mem : in out STMemory);
   function markObjectsAccessibleFrom
     (mem: in out STMemory; objectPointer : in out Pointer) return Pointer;
   procedure rectifyCountsAndDeallocateGarbage (mem : in out STMemory);

   -- ObjectMemory methods for Interpreter
   function fetchPointer
     (mem           : STMemory;
      fieldIndex    : Word;
      objectPointer : Pointer) return Pointer;
   function storePointer
     (mem           : in out STMemory;
      fieldIndex    : Word;
      objectPointer : Pointer;
      valuePointer  : Word) return Word;

   function fetchWord
     (mem           : STMemory;
      wordIndex     : Word;
      objectPointer : Pointer) return Word;
   function storeWord
     (mem           : in out STMemory;
      wordIndex     : Word;
      objectPointer : Pointer;
      valueWord     : Word) return Word;

   function fetchByte
     (mem           : STMemory;
      byteIndex     : Word;
      objectPointer : Pointer) return Byte;
   function storeByte
     (mem           : in out STMemory;
      byteIndex     : Word;
      objectPointer : Pointer;
      valueByte     : Byte) return Byte;

   procedure increaseReferencesTo (mem : in out STMemory; objectPointer : Pointer);
   procedure decreaseReferencesTo (mem : in out STMemory; objectPointer : Pointer);

   function fetchClassOf
     (mem           : STMemory;
      objectPointer : Pointer) return Pointer;

   function fetchWordLengthOf
     (mem           : STMemory;
      objectPointer : Pointer) return Word;
   function fetchByteLengthOf
     (mem           : STMemory;
      objectPointer : Pointer) return Word;

   function instantiateClassWithPointers
     (mem          : in out STMemory;
      classPointer : Pointer;
      length       : Word)
      return Pointer;
   function instantiateClassWithWords
     (mem          : in out STMemory;
      classPointer : Pointer;
      length       : Word)
      return Pointer;
   function instantiateClassWithBytes
     (mem          : in out STMemory;
      classPointer : Pointer;
      length       : Word)
      return Pointer;

   function initialInstanceOf
     (mem          : in out STMemory;
      classPointer : Pointer)
      return Pointer;
   function instanceAfter
     (mem           : STMemory;
      objectPointer : Pointer) return Pointer;

   procedure swapPointers
     (mem       : in out STMemory;
      firstPtr  : Pointer;
      secondPtr : Pointer);

--     procedure isIntegerObject( mem: STMemory; objectPointer : Pointer);
--     procedure cantBeIntegerObject( mem: STMemory; objectPointer : Pointer);
--     procedure xxx( mem: STMemory;);
--     procedure xxx( mem: STMemory;);
--     procedure xxx( mem: STMemory;);
end RSmalltalk.Memory;
