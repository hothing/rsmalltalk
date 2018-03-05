with Interfaces; use Interfaces;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   package  IntTIO renames Ada.Integer_Text_IO;
   package  TextIO renames Ada.Text_IO;

   wch : Wide_Character;

begin

  wch := 'W';
  --wch := 'Ш';
end Main;
