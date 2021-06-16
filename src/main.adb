with Ada.Text_IO;
with Ada.Integer_Text_IO;

with RSmalltalk.Memory; use RSmalltalk.Memory;
with RSmalltalk.Memory.Segmented; use RSmalltalk.Memory.Segmented;

procedure Main is
   package  IntTIO renames Ada.Integer_Text_IO;
   package  TextIO renames Ada.Text_IO;

   sm : T_SegmentedMemory(1);
   vb : T_Byte;
   vw, vw2 : T_Word;
begin
   vw2 := 16#1234#;
   put(sm, 0, 0, vw2);

   vw := get(sm, 0, 0);
   TextIO.Put_Line(T_Word'Image(vw));

   vb := get(sm, 0, 0, 0);
   TextIO.Put_Line(T_Byte'Image(vb));

   vb := get(sm, 0, 0, 1);
   TextIO.Put_Line(T_Byte'Image(vb));

   put(sm, 0, 1, 0, 10);
   put(sm, 0, 1, 1, 20);

   vb := get(sm, 0, 1, 0);
   TextIO.Put_Line(T_Byte'Image(vb));

   vb := get(sm, 0, 1, 1);
   TextIO.Put_Line(T_Byte'Image(vb));

end Main;
