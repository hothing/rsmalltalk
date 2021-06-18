with Interfaces; use Interfaces;
with Ada.Text_IO;

with RSmalltalk.Memory; use RSmalltalk.Memory;
with RSmalltalk.Memory.Heap; use RSmalltalk.Memory.Heap;
with RSmalltalk.Memory.ObjTable; use RSmalltalk.Memory.ObjTable;

procedure Memory_Test is
   package TIO renames Ada.Text_IO;
   mem : T_Memory(4);
   w, w0, w1 : T_Word;
begin
--   w := 1; -- it is valid integer, but not valid pointer
--   putObjectField(mem, 1, w, 0, 0);
--     w := 30;
--     w0 := 1234;
--     putObjectField(mem, 1, w, integerObjectOf(0), w0);
--     w1 := getObjectField(mem, 1, w, integerObjectOf(0));
--     pragma Assert(w0 = w1);
--     TIO.Put_Line(T_Word'Image(w1));
--
--     w := T_Word'Last;
--     w0 := 5678;
--     putObjectField(mem, 1, w, integerObjectOf(10), w0);
--     w1 := getObjectField(mem, 1, w, integerObjectOf(10));
--     pragma Assert(w0 = w1);
--     TIO.Put_Line(T_Word'Image(w1));

   w := 40;
   w0 := integerObjectOf(10);
   putObjectSize(mem, 1, w, w0);
   w1 := getObjectSize(mem, 1, w);
   pragma Assert(w0 = w1);
   TIO.Put_Line(T_Word'Image(w1));

   w := 40;
   w0 := 5678;
   putObjectSize(mem, 1, w, integerObjectOf(10));
   w1 := getObjectField(mem, 1, w, integerObjectOf(0));
   pragma Assert(w0 /= w1);
   TIO.Put_Line(T_Word'Image(w1));

   w := 40;
   w0 := integerObjectOf(10);
   putObjectClass(mem, 1, w, w0);
   w1 := getObjectClass(mem, 1, w);
   pragma Assert(w0 = w1);
   TIO.Put_Line(T_Word'Image(w1));

end Memory_Test;
