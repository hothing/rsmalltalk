with Interfaces; use Interfaces;
with Ada.Text_IO;

with RSmalltalk.Memory; use RSmalltalk.Memory;
with RSmalltalk.Memory.Heap; use RSmalltalk.Memory.Heap;
with RSmalltalk.Memory.ObjTable; use RSmalltalk.Memory.ObjTable;

procedure Memory_Test is
   package TIO renames Ada.Text_IO;
   mem : T_Memory(5);
   sio : T_NumericObject(true);
   spt : T_NumericObject(false);
   w, w0, w1 : T_Word;
   i, j : T_Int;
begin
--     if isSmallIntValue(T_SmallInt'First) and isSmallIntValue(T_SmallInt'Last) then
--        TIO.Put_Line("OK: is small integer");
--     end if;
--
--     if not isSmallIntValue(T_Int'First) then
--        TIO.Put_Line("OK: is not small integer");
--     end if;
--
--     TIO.Put_Line("T_SmallInt'First");
--     i := T_SmallInt'First;
--     TIO.Put_Line(T_Int'Image(i));
--     sio := integerObjectOf(i);
--     TIO.Put_Line(T_Word'Image(rawValueOf(sio)));
--     TIO.Put_Line(T_Int'Image(integerValueOf(sio)));
--
--     TIO.Put_Line("T_SmallInt'Last");
--     i := T_SmallInt'Last;
--     TIO.Put_Line(T_Int'Image(i));
--     sio := integerObjectOf(i);
--     TIO.Put_Line(T_Word'Image(rawValueOf(sio)));
--     TIO.Put_Line(T_Int'Image(integerValueOf(sio)));
--
--     TIO.Put_Line("C_NilPointer");
--     TIO.Put_Line(T_Word'Image(rawValueOf(C_NilPointer)));
--
--     TIO.Put_Line("C_NonPointer as T_Pointer");
--     TIO.Put_Line(T_Word'Image(rawValueOf(C_NonPointer)));
--
--     TIO.Put_Line("Is C_NilPointer valid");
--     TIO.Put_Line(Boolean'Image(isPointerValid(mem, C_NilPointer)));
--     TIO.Put_Line("Is C_NonPointer valid");
--     TIO.Put_Line(Boolean'Image(isPointerValid(mem, C_NonPointer)));
--     TIO.Put_Line("Is Pointer valid");
--     TIO.Put_Line(Boolean'Image(isPointerValid(mem, asPointer(10))));

   TIO.Put_Line("obtainPointer test");
   init(mem);
   for i in 0 .. 32768 loop
      spt := obtainPointer(mem);
      exit when spt = C_NonPointer;
      TIO.Put_Line(T_Word'Image(addressOf(spt)));
   end loop;

end Memory_Test;
