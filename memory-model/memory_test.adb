with RSmalltalk.Memory; use RSmalltalk.Memory;
with RSmalltalk.Memory.Heap; use RSmalltalk.Memory.Heap;
with RSmalltalk.Memory.ObjTable; use RSmalltalk.Memory.ObjTable;
with Interfaces; use Interfaces;
procedure Memory_Test is
   mem : T_Memory(4);
   w, w0, w1 : T_Word;
begin
--   w := 1; -- it is valid integer, but not valid pointer
--   putObjectField(mem, 1, w, 0, 0);
   w0 := 1234;
   putObjectField(mem, 1, 30, integerObjectOf(0), w0);
   w1 := getObjectField(mem, 1, 30, integerObjectOf(0));
   pragma Assert(w0 = w1);
end Memory_Test;
