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
with com_example_Control.Client;
with com_example_ClientServer.Client;
with com_example_Interior.Client;
with org_freedesktop_DBus_Properties.Client;

--  Generated
with D_Bus.Generated_Types; use D_Bus.Generated_Types;

with Shared; use Shared;

procedure Client (Connection : D_Bus.Connection.Connection_Type) is
   --  Types
   type Root is
   new D_Bus.Support.Client.Client_Object
     and org_freedesktop_DBus_Properties.Client.Child_Interface
     and com_example_Control.Client.Child_Interface
     and com_example_ClientServer.Client.Child_Interface with null record;

   type Interior is
   new D_Bus.Support.Client.Client_Object and
     com_example_Interior.Client.Child_Interface with null record;

   --  Objects
   Root_Obj     : aliased Root;
   Interior_Obj : Interior;
begin
   --  Invalid object (Assert_Valid)
   Put_Line ("Use invalid object");
   begin
      pragma Warnings (Off);
      Put_Line (To_String (Root_Obj.Node));
      pragma Warnings (On);
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error => null;
   end;

   --  Create objects
   Root_Obj.Create (Connection, +"/");
   Root_Obj.Register;
   Interior_Obj.Create (Connection, +"/Interior");

   --  Call with no destination
   Put_Line ("Call without destination");
   begin
      Root_Obj.Ping;
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error => null;
   end;

   --  Connect objects
   Root_Obj.Set_Destination ("test.Server");
   Interior_Obj.Set_Destination ("test.Server");

   --  Recreate valid object (Assert_Invalid)
   begin
      Root_Obj.Create (Connection, +"/");
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error => null;
   end;

   --  Node, Connection, Destination
   Put_Line ("Get node, connection, and destination");
   declare
      Node : D_Bus.Types.Obj_Path;
      Connection : D_Bus.Connection.Connection_Type;
      Destination : constant String := Root_Obj.Destination;

      pragma Unreferenced (Node);
      pragma Unreferenced (Connection);
      pragma Unreferenced (Destination);
   begin
      Node := Root_Obj.Node;
      Connection := Root_Obj.Connection;
   end;

   --  Wait for server to go up
   begin
      Root_Obj.Ping;
   exception
      when D_Bus.D_Bus_Error =>
         Root_Obj.Await_Ready;
   end;

   --  Call on invalid interface
   Put_Line ("Call on invalid interface");
   Root_Obj.Call_No_Reply
     (Iface  => "com.example.InvalidInterface",
      Method => "Invalid");

   --  Call invalid method
   Put_Line ("Call invalid method");
   Root_Obj.Call_No_Reply
     (Iface  => "com.example.ClientServer",
      Method => "Invalid");

   --  Call with invalid arguments
   Put_Line ("Call with invalid arguments");
   Root_Obj.Call_No_Reply
     (Iface => "com.example.ClientServer",
      Method => "TestBasicTypes");

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
      FD : GNAT.OS_Lib.File_Descriptor;
   begin
      Root_Obj.TestBasicTypesOut
        (Discard_1, Discard_2, Discard_3, Discard_4, Discard_5, Discard_6,
         Discard_7, Discard_8, Discard_9, Discard_10, Discard_11, Discard_12,
         Discard_13, FD);

      --  Close file descriptor
      GNAT.OS_Lib.Close (FD);
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

      --  Close file descriptors
      for FD of novel_array loop
         GNAT.OS_Lib.Close (FD.Member_1);
         GNAT.OS_Lib.Close (FD.Member_2);
         GNAT.OS_Lib.Close (FD.Member_3);
         GNAT.OS_Lib.Close (FD.Member_4);
         GNAT.OS_Lib.Close (FD.Member_5);
      end loop;
   end;

   --  "in"
   Put_Line ("in");
   Root_Obj.R_in (+"Message");

   --  Interior.Method
   Put_Line ("com.example.Interior.Method @ /Interior");
   Interior_Obj.Method;

   --  Quit
   Put_Line ("Quit");
   Root_Obj.Quit;

   --  Clean up
   Interior_Obj.Destroy;
   Root_Obj.Destroy;
end Client;
