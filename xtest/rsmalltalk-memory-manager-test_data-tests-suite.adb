--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body RSmalltalk.Memory.Manager.Test_Data.Tests.Suite is

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.RSmalltalk.Memory.Manager.Test_Data.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_allocate_3dab65 : aliased Runner_1.Test_Case;
   Case_2_1_Test_deallocate_477675 : aliased Runner_1.Test_Case;
   Case_3_1_Test_free_b6109f : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_allocate_3dab65,
         "rsmalltalk-memory-manager.ads:3:4:",
         Test_allocate_3dab65'Access);
      Runner_1.Create
        (Case_2_1_Test_deallocate_477675,
         "rsmalltalk-memory-manager.ads:5:4:",
         Test_deallocate_477675'Access);
      Runner_1.Create
        (Case_3_1_Test_free_b6109f,
         "rsmalltalk-memory-manager.ads:7:4:",
         Test_free_b6109f'Access);

      Result.Add_Test (Case_1_1_Test_allocate_3dab65'Access);
      Result.Add_Test (Case_2_1_Test_deallocate_477675'Access);
      Result.Add_Test (Case_3_1_Test_free_b6109f'Access);

      return Result'Access;

   end Suite;

end RSmalltalk.Memory.Manager.Test_Data.Tests.Suite;
--  end read only
