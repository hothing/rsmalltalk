with RSmalltalk.Memory.Heap;
with RSmalltalk.Memory.ObjTable;

package body RSmalltalk.Memory.Manager is

   package Heap renames RSmalltalk.Memory.Heap;
   package OT renames RSmalltalk.Memory.ObjTable;

   function allocate(mem: in out T_Memory; size : T_Word) return T_Pointer
   is
   begin
      pragma Compile_Time_Warning(True, "'allocate' unimplemneted");
      return C_NilPointer;
   end allocate;


   procedure deallocate(mem: in out T_Memory; objectPtr : T_Pointer)
   is
   begin
      pragma Compile_Time_Warning(True, "'deallocate' unimplemneted");
      null;
   end deallocate;


   procedure free(mem: in out T_Memory; objectPtr : T_Pointer)
   is
   begin
      pragma Compile_Time_Warning(True, "'free' unimplemneted");
      null;
   end free;

   procedure initFreeChunkList(mem: in out T_Memory; seg : T_SegmentIndex)
   is
      addr : T_Word := Heap.C_LastFreeChunkLocation; --+ Heap.C_ObjectHeaderSize;
      ptr : T_Pointer := C_NilPointer;
   begin
      Heap.putFreeChunkHead(mem, seg, 0, C_NilPointer);
      Heap.putFreeChunkHead(mem, seg, 1, C_NilPointer);
      for sz in T_Word range Heap.C_ObjectHeaderSize .. Heap.C_BigSize loop
         -- get unused pointer from ObjectTable
         ptr := obtainPointer(mem);
         if ptr /= C_NilPointer then
            if ptr /= C_NonPointer then
               addr := addr + sz;
               Heap.makeObjectHeader(mem,
                                     seg,
                                     addr,
                                     integerObjectOf(sz),
                                     ptr);
               Heap.putFreeChunkHead(mem, seg, sz, ptr);
               OT.putLocation(mem, ptr, addr);
            else
               null;
            end if;
         end if;
      end loop;
   end initFreeChunkList;


   -- the procedure makes:
   -- list of free chunk for each heapsegment
   -- list of 'unused' objects in the object table
   procedure initialize(mem: in out T_Memory)
   is
   begin
      pragma Compile_Time_Warning(True, "'initialize' unimplemneted");
      null;
   end initialize;

end RSmalltalk.Memory.Manager;
