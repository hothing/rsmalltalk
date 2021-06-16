package RSmalltalk.Memory.Segmented is

   type T_SegmentedMemory (len : T_SegmentIndex) is private;
   
   function get
     (rwm     : in T_SegmentedMemory;
      segment : in T_SegmentIndex;
      index   : in T_Word) return T_Word;

   function get
     (rwm       : in T_SegmentedMemory;
      segment   : in T_SegmentIndex;
      index     : in T_Word;
      byteIndex : in T_Byte) return T_Byte;

   function put
     (rwm     : in out T_SegmentedMemory;
      segment : in     T_SegmentIndex;
      index   : in     T_Word;
      value   : in     T_Word) return T_Word;
   
   function put
     (rwm       : in out T_SegmentedMemory;
      segment   : in     T_SegmentIndex;
      index     : in     T_Word;
      byteIndex : in     T_Byte;
      value     : in     T_Byte) return T_Byte;

   procedure put
     (rwm     : in out T_SegmentedMemory;
      segment : in     T_SegmentIndex;
      index   : in     T_Word;
      value   : in     T_Word);
   
   procedure put
     (rwm       : in out T_SegmentedMemory;
      segment   : in     T_SegmentIndex;
      index     : in     T_Word;
      byteIndex : in     T_Byte;
      value     : in     T_Byte);

private
   
   type T_BytesInWord is record
      ah : T_Byte;
      al : T_Byte;
   end record;
   for T_BytesInWord'Size use T_Word'Size;
   
   type T_MemorySegment is array(T_Pointer) of T_Word;
     
   type T_MemorySegments is array (T_SegmentIndex range <>) of T_MemorySegment;
   
   type T_SegmentedMemory (len : T_SegmentIndex) is record
      segments : T_MemorySegments (0 .. len);
   end record;

end RSmalltalk.Memory.Segmented;
