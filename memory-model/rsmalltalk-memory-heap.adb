package body RSmalltalk.Memory.Heap is

   function isAddressValid(objectAddress : T_Word) return Boolean
   is
   begin
      return objectAddress > C_LastFreeChunkLocation;
   end isAddressValid;
   pragma Inline_Always(isAddressValid);

   function isAreaValid(objectAddress : T_Word; extraSize : T_Word) return Boolean
   is
   begin
      return (objectAddress > C_LastFreeChunkLocation)
        and (objectAddress <= T_Word'Last - T_Word(integerValueOf(extraSize)));
   end isAreaValid;
   pragma Inline_Always(isAreaValid);

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
      if isAddressValid(objectAddress) then
         w := getObjectSize_unsafe(mem, seg, objectAddress);
         if isIntegerObject(w) and (w >= C_ObjectHeaderSize) then
            return w;
         else
            raise Wrong_Header_Exception;
            return 0;
         end if;
      else
         raise Wrong_Address_Exception;
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
      if isAddressValid(objectAddress) then
         if isIntegerObject(size) then
            putObjectSize_unsafe(mem, seg, objectAddress, size);
         else
            raise Wrong_Parameter_Exception;
         end if;
      else
         raise Wrong_Address_Exception;
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
      if isAreaValid(objectAddress, 1) then
         w_class := T_Word(getObjectClass_unsafe(mem, seg, objectAddress));
         if not isIntegerObject(w_class) then
            return w_class;
         else
            raise Wrong_Header_Exception;
            return C_NonPointer;
         end if;
      else
         raise Wrong_Address_Exception;
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
      if isAreaValid(objectAddress, 1) then
         if not isIntegerObject(T_Word(class)) then
            putObjectClass_unsafe(mem, seg, objectAddress, class);
         else
            raise Wrong_Parameter_Exception;
         end if;
      else
         raise Wrong_Address_Exception;
      end if;
   end putObjectClass;

   function getObjectField_unsafe
     (mem           : T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word;
      fieldIndex    : T_Word
     ) return T_Pointer
   is
   begin
      return get(mem,
                 seg,
                 objectAddress
                 + C_ObjectHeaderSize
                 + T_Word(integerValueOf(fieldIndex)));
   end getObjectField_unsafe;
   pragma Inline_Always(getObjectField_unsafe);

   procedure putObjectField_unsafe
     (mem           : in out T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word;
      fieldIndex    : T_Word;
      fieldObject   : T_Pointer)
   is
   begin
      put(mem,
          seg,
          objectAddress
          + C_ObjectHeaderSize
          + T_Word(integerValueOf(fieldIndex)),
          T_Word(fieldObject));
   end putObjectField_unsafe;
   pragma Inline_Always(putObjectField_unsafe);

   function getObjectField
     (mem           : T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word;
      fieldIndex    : T_Word
     ) return T_Pointer
   is
      w_obj : T_Word;
   begin
      if isAreaValid(objectAddress, fieldIndex + C_ObjectHeaderSize - 1) then
         if isIntegerObject(T_Word(fieldIndex)) then
            w_obj := T_Word(getObjectField_unsafe(mem,
                            seg,
                            objectAddress,
                            fieldIndex));
            return w_obj;
         else
            raise Wrong_Parameter_Exception;
            return C_NonPointer;
         end if;
      else
         raise Wrong_Address_Exception;
         return C_NonPointer;
      end if;
   end getObjectField;

   procedure putObjectField
     (mem           : in out T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word;
      fieldIndex    : T_Word;
      fieldObject   : T_Pointer)
   is
      w_size : T_Word;
   begin
      if isAreaValid(objectAddress, fieldIndex + C_ObjectHeaderSize - 1) then
         if isIntegerObject(T_Word(fieldIndex)) then
            -- validate the fieldIndex
            w_size := getObjectSize(mem, seg, objectAddress);
            if w_size < fieldIndex then
               -- FIXME: condition is a bit bad
               -- should be: integerObject(w_size) - C_ObjectHeaderSize < integerObject(w_size)
               -- it look good, then write
               putObjectField_unsafe(mem,
                                     seg,
                                     objectAddress,
                                     fieldIndex,
                                     fieldObject);
            else
               raise Wrong_Header_Exception;
            end if;
         else
            raise Wrong_Parameter_Exception;
         end if;
      else
         raise Wrong_Address_Exception;
      end if;
   end putObjectField;

   procedure makeObjectHeader(mem : in out T_Memory;
                              seg : T_SegmentIndex; -- segment index
                              objectAddress : T_Word; -- offset in memory
                              size : T_Word; -- size of object
                              classPtr : T_Pointer -- pointeer to the class (in object table)
                             )
   is
   begin
      putObjectSize(mem, seg, objectAddress, size);
      putObjectClass(mem, seg, objectAddress, classPtr);
   end makeObjectHeader;


   function testObjectHeader(mem : in out T_Memory;
                              seg : T_SegmentIndex; -- segment index
                              objectAddress : T_Word -- offset in memory
                             ) return Boolean
   is
      w_size : T_Word;
      w_class : T_Word;
      res : Boolean;
   begin
      res := isAreaValid(objectAddress, 1);
      if res then
         w_size := getObjectSize_unsafe(mem, seg, objectAddress);
         w_class := getObjectClass_unsafe(mem, seg, objectAddress);
         res := isIntegerObject(T_Word(w_size))
           and not isIntegerObject(T_Word(w_class));
         if res then
            res := isAreaValid(objectAddress, w_size);
         end if;
      end if;
      return res;
   end testObjectHeader;

   function getFreeChunkHead
     (mem           : T_Memory;
      seg           : T_SegmentIndex; -- segment index
      size          : T_Word
     ) return T_Pointer
   is
   begin
      if size < C_BigSize then
         return get(mem, seg, C_FirstFreeChunkLocation + size);
      else
         return get(mem, seg, C_LastFreeChunkLocation);
      end if;
   end getFreeChunkHead;


   procedure putFreeChunkHead
     (mem           : in out T_Memory;
      seg           : T_SegmentIndex; -- segment index
      size          : T_Word;
      chunkHead     : T_Pointer)
   is
   begin
      if size < C_BigSize then
         put(mem, seg, C_FirstFreeChunkLocation + size, chunkHead);
      else
         put(mem, seg, C_LastFreeChunkLocation, chunkHead);
      end if;
   end putFreeChunkHead;

end RSmalltalk.Memory.Heap;
