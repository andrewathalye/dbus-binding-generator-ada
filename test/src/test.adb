with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces;

with D_Bus.Arguments.Containers;
with D_Bus.Arguments.Basic; use D_Bus.Arguments.Basic;

with test_Node_test_Node; use test_Node_test_Node;

procedure Test is
   package TN renames test_Node_test_Node;

   V : D_Bus.Arguments.Containers.Variant_Type;

   S : TN.Array_s;
   Sv : TN.Dict_sv;

   Argsvr : TN.Array_rgsvr;
   Oargsvr : TN.Dict_oargsvr;
begin
   TN.Set_Destination ("org.freedesktop.DBus");

   --  Prepare Values
   V := D_Bus.Arguments.Containers.Create (+"Variant");
   S.Append (To_Unbounded_String ("Array_s"));
   Sv.Insert (To_Unbounded_String ("Dict_sv"), V);

   Argsvr.Append
     ((To_Unbounded_String ("a{oa(gsv)}"),
      To_Unbounded_String ("Dict_Oargsvr"),
      V));

   Oargsvr.Insert
     (To_Unbounded_String ("/org/freedesktop/DBus"),
      Argsvr);

   --  TestBasicTypes
   Tn.TestBasicTypes
     (Parameter_1  => 1,
      Parameter_2  => False,
      Parameter_3  => 2,
      Parameter_4  => 3,
      Parameter_5  => 4,
      Parameter_6  => 5,
      Parameter_7  => 6,
      Parameter_8  => 7,
      Parameter_9  => 8.9,
      Parameter_10 => To_Unbounded_String ("unbounded"),
      Parameter_11 => To_Unbounded_String ("/object_path"),
      Parameter_12 => To_Unbounded_String ("ybnqiuxtdsogvh"),
      Parameter_13 => V,
      Parameter_14 => 0);
      
   --  TestComplexTypes
   begin
      TN.TestComplexTypes (
         simple_struct => ((Member_1 => To_Unbounded_String ("Struct_s"))),
         simple_arr => S,
         simple_dict => Sv,
         dict_arr_struct => Oargsvr);
   exception
      when D_Bus.D_Bus_Error => null;
   end;
end Test;
