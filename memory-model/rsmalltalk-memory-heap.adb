package body RSmalltalk.Memory.Heap is

   procedure makeObjectHeader(mem : in out T_Memory;
                              seg : T_SegmentIndex; -- segment index
                              loc : T_Word; -- offset in memory
                              size : T_Word; -- size of object
                              classPtr : T_Pointer -- pointeer to the class (in object table)
                             )
   is
   begin
      if (size >= C_ObjectHeaderSize) and isIntegerObject(T_Word(size)) then
         if loc + size > C_LastFreeChunkLocation then
            if not isIntegerObject(T_Word(classPtr)) then
               put(mem, seg, loc, size);
               put(mem, seg, loc + 1, T_Word(classPtr));
            else
               raise Wrong_Parameter_Exception;
            end if;
         else
            raise Wrong_Parameter_Exception;
         end if;
      else
         raise Wrong_Parameter_Exception;
      end if;
   end makeObjectHeader;


   function testObjectHeader(mem : in out T_Memory;
                              seg : T_SegmentIndex; -- segment index
                              loc : T_Word -- offset in memory
                             ) return Boolean
   is
      w_size : T_Word;
      w_class : T_Word;
   begin
      w_size := get(mem, seg, loc);
      w_class := get(mem, seg, loc + 1);
      return isIntegerObject(T_Word(w_size))
        and not isIntegerObject(T_Word(w_class));
   end testObjectHeader;

   -- object structure manipulation

   function getObjectSize_unsafe
     (mem           : T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word) return T_Word
   is
   begin
      return get(mem, seg, objectAddress);
   end getObjectSize_unsafe;
   pragma Inline_Always(getObjectSize_unsafe);

   function getObjectSize
     (mem           : T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word) return T_Word
   is
      w : T_Word;
   begin
      w := getObjectSize_unsafe(mem, seg, objectAddress);
      if isIntegerObject(w) and (w >= C_ObjectHeaderSize) then
         return w;
      else
         raise Wrong_Header_Exception;
         return 0;
      end if;
   end getObjectSize;

   procedure putObjectSize_unsafe
     (mem           : in out T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word;
      size          : T_Word)
   is
   begin
      put(mem, seg, objectAddress, size);
   end putObjectSize_unsafe;
   pragma Inline_Always(putObjectSize_unsafe);

   procedure putObjectSize
     (mem           : in out T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word;
      size          : T_Word)
   is
   begin
      if isIntegerObject(size) then
         putObjectSize_unsafe(mem, seg, objectAddress, size);
      else
         raise Wrong_Parameter_Exception;
      end if;
   end putObjectSize;

   function getObjectClass_unsafe
     (mem           : T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word) return T_Pointer
   is
   begin
      return get(mem, seg, objectAddress + 1);
   end getObjectClass_unsafe;
   pragma Inline_Always(getObjectClass_unsafe);

   function getObjectClass
     (mem           : T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word) return T_Pointer
   is
      w_class : T_Word;
   begin
      w_class := T_Word(getObjectClass_unsafe(mem, seg, objectAddress));
      if not isIntegerObject(w_class) then
         return w_class;
      else
         raise Wrong_Header_Exception;
         return C_NonPointer;
      end if;
   end getObjectClass;

   procedure putObjectClass_unsafe
     (mem           : in out T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word;
      class         : T_Pointer)
   is
   begin
      put(mem, seg, objectAddress + 1, T_Word(class));
   end putObjectClass_unsafe;
   pragma Inline_Always(putObjectClass_unsafe);

   procedure putObjectClass
     (mem           : in out T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word;
      class         : T_Pointer)
   is
   begin
      if not isIntegerObject(T_Word(class)) then
         putObjectClass_unsafe(mem, seg, objectAddress, class);
      else
         raise Wrong_Parameter_Exception;
      end if;
   end putObjectClass;

end RSmalltalk.Memory.Heap;
