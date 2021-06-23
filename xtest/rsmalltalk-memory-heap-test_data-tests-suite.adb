--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body RSmalltalk.Memory.Heap.Test_Data.Tests.Suite is

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.RSmalltalk.Memory.Heap.Test_Data.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_isAddressValid_8c0d86 : aliased Runner_1.Test_Case;
   Case_2_1_Test_isAreaValid_59da17 : aliased Runner_1.Test_Case;
   Case_3_1_Test_getObjectSize_65aec1 : aliased Runner_1.Test_Case;
   Case_4_1_Test_putObjectSize_50c828 : aliased Runner_1.Test_Case;
   Case_5_1_Test_getObjectClass_2cdfd1 : aliased Runner_1.Test_Case;
   Case_6_1_Test_putObjectClass_3be3f6 : aliased Runner_1.Test_Case;
   Case_7_1_Test_getObjectField_45285b : aliased Runner_1.Test_Case;
   Case_8_1_Test_putObjectField_6a1496 : aliased Runner_1.Test_Case;
   Case_9_1_Test_makeObjectHeader_ebbaa5 : aliased Runner_1.Test_Case;
   Case_10_1_Test_testObjectHeader_c34199 : aliased Runner_1.Test_Case;
   Case_11_1_Test_getFreeChunkHead_b46415 : aliased Runner_1.Test_Case;
   Case_12_1_Test_putFreeChunkHead_2167be : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_isAddressValid_8c0d86,
         "rsmalltalk-memory-heap.ads:16:4:",
         Test_isAddressValid_8c0d86'Access);
      Runner_1.Create
        (Case_2_1_Test_isAreaValid_59da17,
         "rsmalltalk-memory-heap.ads:20:4:",
         Test_isAreaValid_59da17'Access);
      Runner_1.Create
        (Case_3_1_Test_getObjectSize_65aec1,
         "rsmalltalk-memory-heap.ads:23:4:",
         Test_getObjectSize_65aec1'Access);
      Runner_1.Create
        (Case_4_1_Test_putObjectSize_50c828,
         "rsmalltalk-memory-heap.ads:28:4:",
         Test_putObjectSize_50c828'Access);
      Runner_1.Create
        (Case_5_1_Test_getObjectClass_2cdfd1,
         "rsmalltalk-memory-heap.ads:34:4:",
         Test_getObjectClass_2cdfd1'Access);
      Runner_1.Create
        (Case_6_1_Test_putObjectClass_3be3f6,
         "rsmalltalk-memory-heap.ads:39:4:",
         Test_putObjectClass_3be3f6'Access);
      Runner_1.Create
        (Case_7_1_Test_getObjectField_45285b,
         "rsmalltalk-memory-heap.ads:45:4:",
         Test_getObjectField_45285b'Access);
      Runner_1.Create
        (Case_8_1_Test_putObjectField_6a1496,
         "rsmalltalk-memory-heap.ads:52:4:",
         Test_putObjectField_6a1496'Access);
      Runner_1.Create
        (Case_9_1_Test_makeObjectHeader_ebbaa5,
         "rsmalltalk-memory-heap.ads:59:4:",
         Test_makeObjectHeader_ebbaa5'Access);
      Runner_1.Create
        (Case_10_1_Test_testObjectHeader_c34199,
         "rsmalltalk-memory-heap.ads:71:4:",
         Test_testObjectHeader_c34199'Access);
      Runner_1.Create
        (Case_11_1_Test_getFreeChunkHead_b46415,
         "rsmalltalk-memory-heap.ads:76:4:",
         Test_getFreeChunkHead_b46415'Access);
      Runner_1.Create
        (Case_12_1_Test_putFreeChunkHead_2167be,
         "rsmalltalk-memory-heap.ads:82:4:",
         Test_putFreeChunkHead_2167be'Access);

      Result.Add_Test (Case_1_1_Test_isAddressValid_8c0d86'Access);
      Result.Add_Test (Case_2_1_Test_isAreaValid_59da17'Access);
      Result.Add_Test (Case_3_1_Test_getObjectSize_65aec1'Access);
      Result.Add_Test (Case_4_1_Test_putObjectSize_50c828'Access);
      Result.Add_Test (Case_5_1_Test_getObjectClass_2cdfd1'Access);
      Result.Add_Test (Case_6_1_Test_putObjectClass_3be3f6'Access);
      Result.Add_Test (Case_7_1_Test_getObjectField_45285b'Access);
      Result.Add_Test (Case_8_1_Test_putObjectField_6a1496'Access);
      Result.Add_Test (Case_9_1_Test_makeObjectHeader_ebbaa5'Access);
      Result.Add_Test (Case_10_1_Test_testObjectHeader_c34199'Access);
      Result.Add_Test (Case_11_1_Test_getFreeChunkHead_b46415'Access);
      Result.Add_Test (Case_12_1_Test_putFreeChunkHead_2167be'Access);

      return Result'Access;

   end Suite;

end RSmalltalk.Memory.Heap.Test_Data.Tests.Suite;
--  end read only
