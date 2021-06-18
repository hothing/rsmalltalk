package RSmalltalk.Memory.ObjTable is

   subtype T_Counter_5 is T_Byte range 0 .. (2**5 - 1);
   subtype T_Counter_6 is T_Byte range 0 .. (2**5 - 1);
   subtype T_Counter_8 is T_Byte;
   subtype T_Counter_11 is T_Word range 0 .. (2**11 - 1);
   
   type T_ObjectEntryHeader is record
      -- references counter
      count   : T_Counter_8;
      -- pointers field
      ptr     : Boolean;
      -- odd length
      odd     : Boolean;
      -- entry is in a free chunk list
      free    : Boolean;
      -- segment of the referenced object
      segment : T_SegmentIndex;
   end record;
   for T_ObjectEntryHeader use record
      segment at 0 range 0 ..  3; -- see HeapSegmentCount
      ptr     at 0 range 4 ..  4;
      odd     at 0 range 5 ..  5;
      free    at 0 range 6 ..  6;
      count   at 0 range 8 .. 15;
   end record;
   for T_ObjectEntryHeader'Size use T_Word'Size; 
   
   C_OTFreePointerList : constant := 0;
   C_OTStart : constant := C_OTFreePointerList + 1;
   
   Wrong_Parameter_Exception: exception;
   Wrong_Header_Exception: exception;
   
   function getHeader (mem : T_Memory; 
                          objectPointer : T_Pointer 
                         ) return T_ObjectEntryHeader;

   procedure putHeader
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_ObjectEntryHeader);

   function getLocation
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Word;

   procedure putLocation
     (mem           : in out T_Memory;
      objectPointer : T_Pointer;
      value         : T_Word);
   
   function getNextFree
     (mem           : T_Memory;
      objectPointer : T_Pointer) return T_Pointer;
   
end RSmalltalk.Memory.ObjTable;
