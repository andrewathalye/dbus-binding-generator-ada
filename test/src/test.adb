with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces;

with D_Bus.Arguments.Containers;
with D_Bus.Arguments.Basic; use D_Bus.Arguments.Basic;

with test_Node.test_Node; use test_Node.test_Node;

with D_Bus.Generated_Types; use D_Bus.Generated_Types;

procedure Test is
   V : D_Bus.Arguments.Containers.Variant_Type;

   S  : Array_s;
   Sv : Dict_sv;

   Argsvr  : Array_rgsvr;
   Oargsvr : Dict_oargsvr;
begin
   test_Node.Set_Destination ("org.freedesktop.DBus");

   --  Prepare Values
   V := D_Bus.Arguments.Containers.Create (+"Variant");
   S.Append (To_Unbounded_String ("Array_s"));
   Sv.Insert (To_Unbounded_String ("Dict_sv"), V);

   Argsvr.Append
     (Struct_gsv'
        (To_Unbounded_String ("a{oa(gsv)}"),
         To_Unbounded_String ("Dict_Oargsvr"), V));

   Oargsvr.Insert (To_Unbounded_String ("/org/freedesktop/DBus"), Argsvr);

   --  TestBasicTypes
   begin
      TestBasicTypes
        (Parameter_1  => 1, Parameter_2 => False, Parameter_3 => 2,
         Parameter_4  => 3, Parameter_5 => 4, Parameter_6 => 5,
         Parameter_7  => 6, Parameter_8 => 7, Parameter_9 => 8.9,
         Parameter_10 => To_Unbounded_String ("unbounded"),
         Parameter_11 => To_Unbounded_String ("/object_path"),
         Parameter_12 => To_Unbounded_String ("ybnqiuxtdsogvha{s(y)}"),
         Parameter_13 => V, Parameter_14 => 0);
   exception
      when D_Bus.D_Bus_Error =>
         null;
   end;

   --  TestComplexTypes
   begin
      TestComplexTypes
        (simple_struct => ((Member_1 => To_Unbounded_String ("Struct_s"))),
         simple_arr    => S, simple_dict => Sv, dict_arr_struct => Oargsvr);
   exception
      when D_Bus.D_Bus_Error =>
         null;
   end;
end Test;
