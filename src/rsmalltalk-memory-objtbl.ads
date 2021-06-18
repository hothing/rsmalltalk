package RSmalltalk.Memory.ObjTbl is

      -- Object Table start offset(location)
   C_ObjectTableStart   : constant := 10;
   -- Object Table Size
   C_ObjectTableSize    : constant := T_Pointer'Last - C_ObjectTableStart;

   -- The current segment value
   C_CurrentSegment    : constant := 0;
   pragma Compile_Time_Warning(
                               C_ObjectTableStart <= C_CurrentSegment,
                               "Wrong offset of C_ObjectTableStart"
                              );

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

   NonPointerException : exception;
   
   function getObjectRef (mem : T_Memory; objectPointer : T_Pointer) return T_Word;

   procedure putObjectRef
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word);

   function getCountFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word;

   procedure putCountFieldOf
     (mem           : in out T_Memory;
      objectPointer : in T_Pointer;
      value         : in T_Word);

   function getOddFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return Boolean;

   procedure putOddFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : Boolean);

   function getFreeFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return Boolean;

   procedure putFreeFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : Boolean);

   function getPtrFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return Boolean;

   procedure putPtrFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : Boolean);

   function getSegmentFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer)
      return T_SegmentIndex;

   procedure putSegmentFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_SegmentIndex);

   function getLocationFieldOf
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word;

   procedure putLocationFieldOf
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word);
   
   
   
   function getHeadOfFreePointerList (mem : T_Memory) return T_Word;

   procedure putHeadOfFreePointerList
     (mem           : in out T_Memory;
      objectPointer : T_Pointer);

   procedure addToFreePointerList (mem : in out T_Memory;
                                   objectPointer : T_Pointer);

   function removeFromFreePointerList (mem : in out T_Memory) return T_Pointer;
   
   
   function getCurrentSegment (mem : T_Memory) return T_SegmentIndex;

   procedure putCurrentSegment
     (mem           : in out T_Memory;
      value : T_SegmentIndex);

   procedure zeroReferenceCounts (mem : in out T_Memory);

   function obtainPointer(mem : in out T_Memory; 
                          size : T_Word; 
                          location : T_Word
                         ) return T_Pointer;
   
end RSmalltalk.Memory.ObjTbl;
