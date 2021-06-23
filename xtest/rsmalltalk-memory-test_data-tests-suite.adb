--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body RSmalltalk.Memory.Test_Data.Tests.Suite is

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.RSmalltalk.Memory.Test_Data.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_isSmallIntValue_9fb3ca : aliased Runner_1.Test_Case;
   Case_2_1_Test_isSmallWordValue_b737f5 : aliased Runner_1.Test_Case;
   Case_3_1_Test_isIntegerObject_3fe95a : aliased Runner_1.Test_Case;
   Case_4_1_Test_integerValueOf_ae2006 : aliased Runner_1.Test_Case;
   Case_5_1_Test_wordValueOf_c1bc3d : aliased Runner_1.Test_Case;
   Case_6_1_Test_integerObjectOf_d08c18 : aliased Runner_1.Test_Case;
   Case_7_1_Test_integerObjectOf_50721f : aliased Runner_1.Test_Case;
   Case_8_1_Test_asIntegerObject_538866 : aliased Runner_1.Test_Case;
   Case_9_1_Test_get_422982 : aliased Runner_1.Test_Case;
   Case_10_1_Test_get_fecc58 : aliased Runner_1.Test_Case;
   Case_11_1_Test_put_d55f44 : aliased Runner_1.Test_Case;
   Case_12_1_Test_put_a7c9ed : aliased Runner_1.Test_Case;
   Case_13_1_Test_put_862726 : aliased Runner_1.Test_Case;
   Case_14_1_Test_put_a48197 : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_isSmallIntValue_9fb3ca,
         "rsmalltalk-memory.ads:44:4:",
         Test_isSmallIntValue_9fb3ca'Access);
      Runner_1.Create
        (Case_2_1_Test_isSmallWordValue_b737f5,
         "rsmalltalk-memory.ads:46:4:",
         Test_isSmallWordValue_b737f5'Access);
      Runner_1.Create
        (Case_3_1_Test_isIntegerObject_3fe95a,
         "rsmalltalk-memory.ads:48:4:",
         Test_isIntegerObject_3fe95a'Access);
      Runner_1.Create
        (Case_4_1_Test_integerValueOf_ae2006,
         "rsmalltalk-memory.ads:50:4:",
         Test_integerValueOf_ae2006'Access);
      Runner_1.Create
        (Case_5_1_Test_wordValueOf_c1bc3d,
         "rsmalltalk-memory.ads:52:4:",
         Test_wordValueOf_c1bc3d'Access);
      Runner_1.Create
        (Case_6_1_Test_integerObjectOf_d08c18,
         "rsmalltalk-memory.ads:54:4:",
         Test_integerObjectOf_d08c18'Access);
      Runner_1.Create
        (Case_7_1_Test_integerObjectOf_50721f,
         "rsmalltalk-memory.ads:56:4:",
         Test_integerObjectOf_50721f'Access);
      Runner_1.Create
        (Case_8_1_Test_asIntegerObject_538866,
         "rsmalltalk-memory.ads:58:4:",
         Test_asIntegerObject_538866'Access);
      Runner_1.Create
        (Case_9_1_Test_get_422982,
         "rsmalltalk-memory.ads:70:4:",
         Test_get_422982'Access);
      Runner_1.Create
        (Case_10_1_Test_get_fecc58,
         "rsmalltalk-memory.ads:75:4:",
         Test_get_fecc58'Access);
      Runner_1.Create
        (Case_11_1_Test_put_d55f44,
         "rsmalltalk-memory.ads:81:4:",
         Test_put_d55f44'Access);
      Runner_1.Create
        (Case_12_1_Test_put_a7c9ed,
         "rsmalltalk-memory.ads:87:4:",
         Test_put_a7c9ed'Access);
      Runner_1.Create
        (Case_13_1_Test_put_862726,
         "rsmalltalk-memory.ads:94:4:",
         Test_put_862726'Access);
      Runner_1.Create
        (Case_14_1_Test_put_a48197,
         "rsmalltalk-memory.ads:100:4:",
         Test_put_a48197'Access);

      Result.Add_Test (Case_1_1_Test_isSmallIntValue_9fb3ca'Access);
      Result.Add_Test (Case_2_1_Test_isSmallWordValue_b737f5'Access);
      Result.Add_Test (Case_3_1_Test_isIntegerObject_3fe95a'Access);
      Result.Add_Test (Case_4_1_Test_integerValueOf_ae2006'Access);
      Result.Add_Test (Case_5_1_Test_wordValueOf_c1bc3d'Access);
      Result.Add_Test (Case_6_1_Test_integerObjectOf_d08c18'Access);
      Result.Add_Test (Case_7_1_Test_integerObjectOf_50721f'Access);
      Result.Add_Test (Case_8_1_Test_asIntegerObject_538866'Access);
      Result.Add_Test (Case_9_1_Test_get_422982'Access);
      Result.Add_Test (Case_10_1_Test_get_fecc58'Access);
      Result.Add_Test (Case_11_1_Test_put_d55f44'Access);
      Result.Add_Test (Case_12_1_Test_put_a7c9ed'Access);
      Result.Add_Test (Case_13_1_Test_put_862726'Access);
      Result.Add_Test (Case_14_1_Test_put_a48197'Access);

      return Result'Access;

   end Suite;

end RSmalltalk.Memory.Test_Data.Tests.Suite;
--  end read only
