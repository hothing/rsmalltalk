--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body RSmalltalk.Memory.ObjTable.Test_Data.Tests.Suite is

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.RSmalltalk.Memory.ObjTable.Test_Data.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_getHeader_c4bc16 : aliased Runner_1.Test_Case;
   Case_2_1_Test_putHeader_e46893 : aliased Runner_1.Test_Case;
   Case_3_1_Test_getLocation_6e173c : aliased Runner_1.Test_Case;
   Case_4_1_Test_putLocation_17edcc : aliased Runner_1.Test_Case;
   Case_5_1_Test_getNextFree_d10510 : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_getHeader_c4bc16,
         "rsmalltalk-memory-objtable.ads:35:4:",
         Test_getHeader_c4bc16'Access);
      Runner_1.Create
        (Case_2_1_Test_putHeader_e46893,
         "rsmalltalk-memory-objtable.ads:39:4:",
         Test_putHeader_e46893'Access);
      Runner_1.Create
        (Case_3_1_Test_getLocation_6e173c,
         "rsmalltalk-memory-objtable.ads:44:4:",
         Test_getLocation_6e173c'Access);
      Runner_1.Create
        (Case_4_1_Test_putLocation_17edcc,
         "rsmalltalk-memory-objtable.ads:48:4:",
         Test_putLocation_17edcc'Access);
      Runner_1.Create
        (Case_5_1_Test_getNextFree_d10510,
         "rsmalltalk-memory-objtable.ads:53:4:",
         Test_getNextFree_d10510'Access);

      Result.Add_Test (Case_1_1_Test_getHeader_c4bc16'Access);
      Result.Add_Test (Case_2_1_Test_putHeader_e46893'Access);
      Result.Add_Test (Case_3_1_Test_getLocation_6e173c'Access);
      Result.Add_Test (Case_4_1_Test_putLocation_17edcc'Access);
      Result.Add_Test (Case_5_1_Test_getNextFree_d10510'Access);

      return Result'Access;

   end Suite;

end RSmalltalk.Memory.ObjTable.Test_Data.Tests.Suite;
--  end read only
