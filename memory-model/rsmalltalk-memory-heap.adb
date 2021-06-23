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
        and (objectAddress <= T_Word'Last - extraSize);
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
         if (w >= C_ObjectHeaderSize) then
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
         if size >= C_ObjectHeaderSize then
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
      w : T_Word := get(mem, seg, objectAddress + 1);
   begin
      return asPointer(w);
   end getObjectClass_unsafe;
   pragma Inline_Always(getObjectClass_unsafe);

   function getObjectClass
     (mem           : T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word) return T_Pointer
   is
   begin
      if isAreaValid(objectAddress, 1) then
         return getObjectClass_unsafe(mem, seg, objectAddress);
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
      put(mem, seg, objectAddress + 1, rawValueOf(class));
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
         if not isIntegerObject(class) then
            putObjectClass_unsafe(mem, seg, objectAddress, class);
         else
            raise Wrong_Parameter_Exception;
         end if;
      else
         raise Wrong_Address_Exception;
      end if;
   end putObjectClass;

   function isFieldIndexValid
     (mem           : T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word;
      fieldIndex    : T_Word) return Boolean
   is
      obj_size : T_Word;
   begin
      obj_size := getObjectSize(mem, seg, objectAddress);
      return isAreaValid(objectAddress, fieldIndex + C_ObjectHeaderSize - 1)
        and (obj_size > C_ObjectHeaderSize)
        and (fieldIndex < obj_size);
   end;

   function getObjectField_unsafe
     (mem           : T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word;
      fieldIndex    : T_Word
     ) return T_Pointer
   is
   begin
      return asPointer(get(mem,
                       seg,
                       objectAddress
                       + C_ObjectHeaderSize
                       + fieldIndex));
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
          + fieldIndex,
          rawValueOf(fieldObject));
   end putObjectField_unsafe;
   pragma Inline_Always(putObjectField_unsafe);

   function getObjectField
     (mem           : T_Memory;
      seg           : T_SegmentIndex; -- segment index
      objectAddress : T_Word;
      fieldIndex    : T_Word
     ) return T_Pointer
   is
   begin
      if isFieldIndexValid(mem, seg, objectAddress, fieldIndex) then
         return getObjectField_unsafe(mem,
                            seg,
                            objectAddress,
                            fieldIndex);
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
   begin
      if isFieldIndexValid(mem, seg, objectAddress, fieldIndex) then
         putObjectField_unsafe(mem,
                                     seg,
                                     objectAddress,
                                     fieldIndex,
                                     fieldObject);
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
         w_size := get(mem, seg, objectAddress);
         w_class := get(mem, seg, objectAddress + 1);
         res := isIntegerObject(w_size)
           and not isIntegerObject(w_class);
         if res then
            res := isAreaValid(objectAddress, w_size);
         end if;
      end if;
      return res;
   end testObjectHeader;

   function isPointerInCell(mem : in out T_Memory;
                      seg : T_SegmentIndex; -- segment index
                      objectAddress : T_Word -- offset in memory
                     ) return Boolean
   is
      w : T_Word;
      res : Boolean;
   begin
      res := isAreaValid(objectAddress, 1);
      if res then
         w := get(mem, seg, objectAddress);
         res := not isIntegerObject(w);
      end if;
      return res;
   end isPointerInCell;


   function getFreeChunkHead
     (mem           : T_Memory;
      seg           : T_SegmentIndex; -- segment index
      size          : T_Word
     ) return T_Pointer
   is
      w : T_Word;
   begin
      if size < C_BigSize then
         w := get(mem, seg, C_FirstFreeChunkLocation + size);
      else
         w := get(mem, seg, C_LastFreeChunkLocation);
      end if;
      if isIntegerObject(w) then
         return asPointer(w);
      else
         raise Wrong_Value_Exception;
         return C_NilPointer;
      end if;
   end getFreeChunkHead;


   procedure putFreeChunkHead
     (mem           : in out T_Memory;
      seg           : T_SegmentIndex; -- segment index
      size          : T_Word;
      chunkHead     : T_Pointer)
   is
      p : T_Word := wordValueOf(chunkHead);
   begin
      if size < C_BigSize then
         put(mem, seg, C_FirstFreeChunkLocation + size, p);
      else
         put(mem, seg, C_LastFreeChunkLocation, p);
      end if;
   end putFreeChunkHead;

end RSmalltalk.Memory.Heap;
