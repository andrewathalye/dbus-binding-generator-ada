with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces;

with D_Bus.Arguments.Containers;
with D_Bus.Arguments.Basic; use D_Bus.Arguments.Basic;

with test_interface;

procedure Test is
   V : D_Bus.Arguments.Containers.Variant_Type;

   S : test_interface.Array_s;
   Sv : test_interface.Dict_sv;
   Rtsr : test_interface.Array_rtsr;
   Trsvr : test_interface.Dict_trsvr;

   Argsvr : test_interface.Array_rgsvr;
   Oargsvr : test_interface.Dict_oargsvr;
begin
   --  For the sake of testing
   test_interface.Connect ("org.freedesktop.DBus");

   V := D_Bus.Arguments.Containers.Create (+"Variant");
   S.Append (To_Unbounded_String ("Array_s"));
   Sv.Insert (To_Unbounded_String ("Dict_sv"), V);
   Rtsr.Append ((100, To_Unbounded_String ("Array_rtsr")));
   Trsvr.Insert (100, (To_Unbounded_String ("Dict_trsvr"), V));

   Argsvr.Append
     ((To_Unbounded_String ("a{oa(gsv)}"),
      To_Unbounded_String ("Dict_Oargsvr"),
      V));

   Oargsvr.Insert
     (To_Unbounded_String ("/org/freedesktop/DBus"),
      Argsvr);
      

   test_interface.TestComplexTypes (
      simple_arr => S,
      simple_dict => Sv,
      arr_struct => Rtsr,
      dict_struct => Trsvr,
      dict_arr_struct => Oargsvr);
end Test;
