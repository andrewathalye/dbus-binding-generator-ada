pragma Ada_2012;

with Ada.Numerics;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;
with GNAT.OS_Lib;

--  D_Bus
with D_Bus.Arguments.Containers; use D_Bus.Arguments.Containers;
with D_Bus.Arguments.Basic;      use D_Bus.Arguments.Basic;
with D_Bus.Types;                use D_Bus.Types;

--  Support
with D_Bus.Support.Client; use D_Bus.Support.Client;

--  Interfaces
with tk_zenithseeker_Control.Client; use tk_zenithseeker_Control.Client;
with com_example_Interface.Client;   use com_example_Interface.Client;
with com_example_Interior.Client;    use com_example_Interior.Client;

--  Generated
with D_Bus.Generated_Types; use D_Bus.Generated_Types;

procedure Bindings_Client is
   --  Renamings
   function "+" (L : String) return Unbounded_String renames
     To_Unbounded_String;
   function "+" (L : Unbounded_String) return String renames To_String;

   --  Types
   type Root is
   new D_Bus.Support.Client.Client_Object and
     tk_zenithseeker_Control.Client.Child_Interface and
     com_example_Interface.Client.Child_Interface with null record;

   type Interior is
   new D_Bus.Support.Client.Client_Object and
     com_example_Interior.Client.Child_Interface with null record;

   --  Objects
   Root_Obj     : Root;
   Interior_Obj : Interior;
begin
   --  Create objects
   Root_Obj.Create (+"/");
   Interior_Obj.Create (+"/Interior");

   --  Connect objects
   Root_Obj.Set_Destination ("test.Service");
   Interior_Obj.Set_Destination ("test.Service");

   Put_Line ("com.example.Interface @ /");

   --  TestEmpty
   Put_Line ("TestEmpty");
   Root_Obj.TestEmpty;

   --  TestBasicTypes
   Put_Line ("TestBasicTypes");
   Root_Obj.TestBasicTypes
     (Parameter_1  => 0, Parameter_2 => True,
      Parameter_3  => Interfaces.Integer_16'First,
      Parameter_4  => Interfaces.Unsigned_16'Last,
      Parameter_5  => Interfaces.Integer_32'First,
      Parameter_6  => Interfaces.Unsigned_32'Last,
      Parameter_7  => Interfaces.Integer_64'First,
      Parameter_8  => Interfaces.Unsigned_64'Last,
      Parameter_9  => Ada.Numerics.Pi, Parameter_10 => +"String",
      Parameter_11 => +"/ObjPath", Parameter_12 => +"signatu",
      Parameter_13 => D_Bus.Arguments.Containers.Create (+"Variant"),
      Parameter_14 => 0);

   --  TestBasicTypesOut
   Put_Line ("TestBasicTypesOut");
   declare
      Discard_1  : Interfaces.Unsigned_8;
      Discard_2  : Boolean;
      Discard_3  : Interfaces.Integer_16;
      Discard_4  : Interfaces.Unsigned_16;
      Discard_5  : Interfaces.Integer_32;
      Discard_6  : Interfaces.Unsigned_32;
      Discard_7  : Interfaces.Integer_64;
      Discard_8  : Interfaces.Unsigned_64;
      Discard_9  : Interfaces.IEEE_Float_64;
      Discard_10 : Ada.Strings.Unbounded.Unbounded_String;
      Discard_11 : D_Bus.Types.Obj_Path;
      Discard_12 : D_Bus.Types.Signature;
      Discard_13 : D_Bus.Arguments.Containers.Variant_Type;
      Discard_14 : GNAT.OS_Lib.File_Descriptor;
   begin
      Root_Obj.TestBasicTypesOut
        (Discard_1, Discard_2, Discard_3, Discard_4, Discard_5, Discard_6,
         Discard_7, Discard_8, Discard_9, Discard_10, Discard_11, Discard_12,
         Discard_13, Discard_14);
   end;

   --  Complex types
   declare
      simple_struct  : Struct_s;
      simple_arr     : Array_s;
      int_dict       : Dict_iv;
      string_dict    : Dict_sv;
      obj_path_dict  : Dict_ov;
      signature_dict : Dict_gv;
      nested_dict    : Dict_saesve;
      nested_struct  : Struct_rsr;
      novel_array    : Array_rhhhhhr;
   begin
      --  Set values
      simple_struct := (Member_1 => +"simple_struct");
      simple_arr.Append (+"simple_arr");
      int_dict.Insert (0, Create (+"int_dict"));
      string_dict.Insert (+"key", Create (+"string_dict"));
      obj_path_dict.Insert (+"/Key", Create (+"obj_path_dict"));
      signature_dict.Insert (+"signatu", Create (+"signature_dict"));
      nested_dict.Insert (+"key", string_dict);
      nested_struct := (Member_1 => Struct_s'(Member_1 => +"nested_struct"));
      novel_array.Append ((0, 0, 0, 0, 0));

      --  TestComplexTypes
      Put_Line ("TestComplexTypes");
      Root_Obj.TestComplexTypes
        (simple_struct => simple_struct, simple_arr => simple_arr,
         int_dict      => int_dict, string_dict => string_dict,
         obj_path_dict => obj_path_dict, signature_dict => signature_dict,
         nested_dict   => nested_dict, nested_struct => nested_struct,
         novel_array   => novel_array);

      --  TestComplexTypesOut
      Put_Line ("TestComplexTypesOut");
      Root_Obj.TestComplexTypesOut
        (simple_struct => simple_struct, simple_arr => simple_arr,
         int_dict      => int_dict, string_dict => string_dict,
         obj_path_dict => obj_path_dict, signature_dict => signature_dict,
         nested_dict   => nested_dict, nested_struct => nested_struct,
         novel_array   => novel_array);
   end;

   --  "in"
   Put_Line ("in");
   Root_Obj.R_in (+"Message");

   --  Properties
   Put_Line ("Properties (org.freedesktop.DBus.Properties) @ /");
   declare
      Buf : Unbounded_String;
   begin
      Buf := Root_Obj.TestPropertyReadOnly;
      Root_Obj.Set_TestPropertyWriteOnly (Buf);
      Buf := Root_Obj.TestProperty;
      Root_Obj.Set_TestProperty (Buf);
   end;

   --  Interior.Method
   Put_Line ("com.example.Interior.Method @ /Interior");
   Interior_Obj.Method;

   -----------------------------
   -- tk.zenithseeker.Control --
   -----------------------------
   Put_Line ("tk.zenithseeker.Control @ /");

   --  Quit
   declare
      Discard : Unbounded_String;
   begin
      Put_Line ("Register QuitSignal");
      Root_Obj.Register_QuitSignal;

      Put_Line ("Quit");
      Root_Obj.Quit;

      Put_Line ("Await QuitSignal");
      Root_Obj.Await_QuitSignal (Discard);

      Put_Line ("Unregister QuitSignal");
      Root_Obj.Unregister_QuitSignal;
   end;

   --  Clean up
   Interior_Obj.Destroy;
   Root_Obj.Destroy;
end Bindings_Client;
