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

   function obtainPointer(mem : T_Memory) return T_Pointer
   is
   begin
      pragma Compile_Time_Warning(True, "'obtainPointer' unimplemneted");
      return C_NilPointer;
   end obtainPointer;

   procedure initFreeChunkList(mem: in out T_Memory; seg : T_SegmentIndex)
   is
      addr : T_Word := Heap.C_LastFreeChunkLocation + 1;
      ptr : T_Pointer := C_NilPointer;
   begin
      for sz in T_Word range 2 .. Heap.C_BigSize loop
         -- get unused pointer from ObjectTable
         ptr := obtainPointer(mem);
         if ptr /= C_NilPointer then
            Heap.putFreeChunkHead(mem, seg, sz, ptr);
            OT.putLocation(mem, ptr, addr);
            addr := addr + sz + Heap.C_ObjectHeaderSize;
         end if;
      end loop;
   end initFreeChunkList;


   -- the procedure makes:
   -- list of free chunk for each heapsegment
   -- list of 'unused' objects in the object table
   procedure initialize(mem: in out T_Memory)
   is
   begin
      null;
   end initialize;

end RSmalltalk.Memory.Manager;
