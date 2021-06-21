with Interfaces; use Interfaces;
with Ada.Text_IO;

with RSmalltalk.Memory; use RSmalltalk.Memory;
with RSmalltalk.Memory.Heap; use RSmalltalk.Memory.Heap;
with RSmalltalk.Memory.ObjTable; use RSmalltalk.Memory.ObjTable;

procedure Memory_Test is
   package TIO renames Ada.Text_IO;
   mem : T_Memory(4);
   w, w0, w1 : T_Word;
   i, j : T_Int;
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

--     w := 40;
--     w0 := integerObjectOf(10);
--     putObjectSize(mem, 1, w, w0);
--     w1 := getObjectSize(mem, 1, w);
--     pragma Assert(w0 = w1);
--     TIO.Put_Line(T_Word'Image(w1));
--
--     w := 40;
--     w0 := 5678;
--     putObjectSize(mem, 1, w, integerObjectOf(10));
--     w1 := getObjectField(mem, 1, w, integerObjectOf(0));
--     pragma Assert(w0 /= w1);
--     TIO.Put_Line(T_Word'Image(w1));
--
--     w := 40;
--     w0 := integerObjectOf(10);
--     putObjectClass(mem, 1, w, w0);
--     w1 := getObjectClass(mem, 1, w);
--     pragma Assert(w0 = w1);
--     TIO.Put_Line(T_Word'Image(w1));
--
--     w := 40;
--     w0 := integerObjectOf(10);
--     makeObjectHeader(mem, 1, w, w0, C_NilPointer);
--     w1 := getObjectSize(mem, 1, w);
--     pragma Assert(w0 = w1);
--     TIO.Put_Line(T_Word'Image(w1));
--     w1 := getObjectClass(mem, 1, w);
--     pragma Assert(C_NilPointer = w1);
--     TIO.Put_Line(T_Word'Image(w1));

--     w := T_Word'Last - C_ObjectHeaderSize;
--     w0 := integerObjectOf(T_Word(10));
--     makeObjectHeader(mem, 1, w - C_ObjectHeaderSize, integerObjectOf(T_Word(2)), C_NilPointer);
--     putObjectField(mem, 1, w - C_ObjectHeaderSize, integerObjectOf(T_Word(0)), w0);
--     putObjectField(mem, 1, w - C_ObjectHeaderSize, integerObjectOf(T_Word(1)), C_NilPointer);
--     if testObjectHeader(mem, 1, w) then
--        TIO.Put_Line("bad");
--     else
--        TIO.Put_Line("OK: wrong header");
--     end if;

   if isSmallIntValue(T_SmallInt'First) and isSmallIntValue(T_SmallInt'Last) then
      TIO.Put_Line("OK: is small integer");
   end if;

   if not isSmallIntValue(T_Int'First) then
      TIO.Put_Line("OK: is not small integer");
   end if;

   TIO.Put_Line("T_SmallInt'First");
   i := T_SmallInt'First;
   TIO.Put_Line(T_Int'Image(i));
   w := integerObjectOf(i);
   TIO.Put_Line(T_Int'Image(integerValueOf(w)));

   TIO.Put_Line("T_SmallInt'Last");
   i := T_SmallInt'Last;
   TIO.Put_Line(T_Int'Image(i));
   w := integerObjectOf(i);
   TIO.Put_Line(T_Int'Image(integerValueOf(w)));

   TIO.Put_Line("ptr = 3 <= 1*2 + 1");
   w := 16#3#;
   TIO.Put_Line(T_Int'Image(integerValueOf(w)));

   TIO.Put_Line("ptr = 2001h");
   w := 16#2001#;
   TIO.Put_Line(T_Word'Image(wordValueOf(w)));
   TIO.Put_Line(T_Int'Image(integerValueOf(w)));

   TIO.Put_Line("ptr = 8000h");
   w := 16#8000#;
   TIO.Put_Line(T_Word'Image(wordValueOf(w)));
   TIO.Put_Line(T_Int'Image(integerValueOf(w)));

end Memory_Test;
