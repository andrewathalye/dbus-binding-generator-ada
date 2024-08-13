pragma Ada_2012;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;
with GNAT.OS_Lib;

--  D_Bus
with D_Bus.Arguments.Containers;
with D_Bus.Arguments.Basic;
with D_Bus.Connection.G_Main;
with D_Bus.G_Main;
with D_Bus.Types; use D_Bus.Types;

--  Support
with D_Bus.Support.Server; use D_Bus.Support.Server;

--  Interfaces
with com_example_Control.Server;
with com_example_Duplicate.Server;
with com_example_ClientServer.Server;
with com_example_Interior.Server;
with com_example_InteriorWithEmpty.Server;

--  Generated
with D_Bus.Generated_Objects; use D_Bus.Generated_Objects;
with D_Bus.Generated_Types;   use D_Bus.Generated_Types;

with Shared; use Shared;

procedure Server (Connection : D_Bus.Connection.Connection_Type) is
   --  Types
   type Root is
   new D_Bus.Support.Server.Server_Object and
     com_example_Control.Server.Child_Interface and
     com_example_Duplicate.Server.Child_Interface and
     com_example_ClientServer.Server.Child_Interface with null record;

   type Interior is
   new D_Bus.Support.Server.Server_Object and
     com_example_Interior.Server.Child_Interface with null record;

   type InteriorWithEmpty is
   new D_Bus.Support.Server.Server_Object and
     com_example_InteriorWithEmpty.Server.Child_Interface with null record;

   type Empty is new D_Bus.Support.Server.Server_Object with null record;

   --  Override Specs
   overriding procedure TestBasicTypes
     (O            : in out Root; Parameter_1 : Interfaces.Unsigned_8;
      Parameter_2  :  Boolean; Parameter_3 : Interfaces.Integer_16;
      Parameter_4  :  Interfaces.Unsigned_16;
      Parameter_5  :  Interfaces.Integer_32;
      Parameter_6  :  Interfaces.Unsigned_32;
      Parameter_7  :  Interfaces.Integer_64;
      Parameter_8  :  Interfaces.Unsigned_64;
      Parameter_9  :  Interfaces.IEEE_Float_64;
      Parameter_10 :  Ada.Strings.Unbounded.Unbounded_String;
      Parameter_11 :  D_Bus.Types.Obj_Path;
      Parameter_12 :  D_Bus.Types.Signature;
      Parameter_13 :  D_Bus.Arguments.Containers.Variant_Type;
      Parameter_14 :  GNAT.OS_Lib.File_Descriptor);

   overriding procedure TestBasicTypesOut
     (O            : in out Root; Parameter_1 : out Interfaces.Unsigned_8;
      Parameter_2  :    out Boolean; Parameter_3 : out Interfaces.Integer_16;
      Parameter_4  :    out Interfaces.Unsigned_16;
      Parameter_5  :    out Interfaces.Integer_32;
      Parameter_6  :    out Interfaces.Unsigned_32;
      Parameter_7  :    out Interfaces.Integer_64;
      Parameter_8  :    out Interfaces.Unsigned_64;
      Parameter_9  :    out Interfaces.IEEE_Float_64;
      Parameter_10 :    out Ada.Strings.Unbounded.Unbounded_String;
      Parameter_11 :    out D_Bus.Types.Obj_Path;
      Parameter_12 :    out D_Bus.Types.Signature;
      Parameter_13 :    out D_Bus.Arguments.Containers.Variant_Type;
      Parameter_14 :    out GNAT.OS_Lib.File_Descriptor);

   overriding procedure TestComplexTypes
     (O : in out Root; simple_struct :  Struct_s; simple_arr :  Array_s;
      int_dict      :     Dict_iv; string_dict :  Dict_sv;
      obj_path_dict :     Dict_ov; signature_dict :  Dict_gv;
      nested_dict   :     Dict_saesve; nested_struct :  Struct_rsr;
      novel_array   :     Array_rhhhhhr);

   overriding procedure TestComplexTypesOut
     (O : in out Root; simple_struct : out Struct_s; simple_arr : out Array_s;
      int_dict      :    out Dict_iv; string_dict : out Dict_sv;
      obj_path_dict :    out Dict_ov; signature_dict : out Dict_gv;
      nested_dict   :    out Dict_saesve; nested_struct : out Struct_rsr;
      novel_array   :    out Array_rhhhhhr);

   overriding procedure Quit (O : in out Root);

   --  Override Implementation
   pragma Warnings (Off, "-gnatwf");
   overriding procedure TestBasicTypes
     (O            : in out Root; Parameter_1 : Interfaces.Unsigned_8;
      Parameter_2  :  Boolean; Parameter_3 : Interfaces.Integer_16;
      Parameter_4  :  Interfaces.Unsigned_16;
      Parameter_5  :  Interfaces.Integer_32;
      Parameter_6  :  Interfaces.Unsigned_32;
      Parameter_7  :  Interfaces.Integer_64;
      Parameter_8  :  Interfaces.Unsigned_64;
      Parameter_9  :  Interfaces.IEEE_Float_64;
      Parameter_10 :  Ada.Strings.Unbounded.Unbounded_String;
      Parameter_11 :  D_Bus.Types.Obj_Path;
      Parameter_12 :  D_Bus.Types.Signature;
      Parameter_13 :  D_Bus.Arguments.Containers.Variant_Type;
      Parameter_14 :  GNAT.OS_Lib.File_Descriptor)
   is
   begin
      GNAT.OS_Lib.Close (Parameter_14);
   end TestBasicTypes;

   overriding procedure TestBasicTypesOut
     (O            : in out Root; Parameter_1 : out Interfaces.Unsigned_8;
      Parameter_2  :    out Boolean; Parameter_3 : out Interfaces.Integer_16;
      Parameter_4  :    out Interfaces.Unsigned_16;
      Parameter_5  :    out Interfaces.Integer_32;
      Parameter_6  :    out Interfaces.Unsigned_32;
      Parameter_7  :    out Interfaces.Integer_64;
      Parameter_8  :    out Interfaces.Unsigned_64;
      Parameter_9  :    out Interfaces.IEEE_Float_64;
      Parameter_10 :    out Ada.Strings.Unbounded.Unbounded_String;
      Parameter_11 :    out D_Bus.Types.Obj_Path;
      Parameter_12 :    out D_Bus.Types.Signature;
      Parameter_13 :    out D_Bus.Arguments.Containers.Variant_Type;
      Parameter_14 :    out GNAT.OS_Lib.File_Descriptor)
   is
      use type D_Bus.Arguments.Basic.String_Type;
   begin
      Parameter_13 := D_Bus.Arguments.Containers.Create (+"Variant");
   end TestBasicTypesOut;

   overriding procedure TestComplexTypes
     (O : in out Root; simple_struct :  Struct_s; simple_arr :  Array_s;
      int_dict      :     Dict_iv; string_dict :  Dict_sv;
      obj_path_dict :     Dict_ov; signature_dict :  Dict_gv;
      nested_dict   :     Dict_saesve; nested_struct :  Struct_rsr;
      novel_array   :     Array_rhhhhhr)
   is
   begin
      for FD of novel_array loop
         GNAT.OS_Lib.Close (FD.Member_1);
         GNAT.OS_Lib.Close (FD.Member_2);
         GNAT.OS_Lib.Close (FD.Member_3);
         GNAT.OS_Lib.Close (FD.Member_4);
         GNAT.OS_Lib.Close (FD.Member_5);
      end loop;
   end TestComplexTypes;

   overriding procedure TestComplexTypesOut
     (O : in out Root; simple_struct : out Struct_s; simple_arr : out Array_s;
      int_dict      :    out Dict_iv; string_dict : out Dict_sv;
      obj_path_dict :    out Dict_ov; signature_dict : out Dict_gv;
      nested_dict   :    out Dict_saesve; nested_struct : out Struct_rsr;
      novel_array   :    out Array_rhhhhhr)
   is
      Struct : constant Struct_hhhhh := (others => 0);
   begin
      --  Make sure we send out some file descriptors
      novel_array.Append (Struct);
   end TestComplexTypesOut;

   pragma Warnings (On);

   overriding procedure Quit (O : in out Root) is
   begin
      D_Bus.G_Main.Quit;
   end Quit;

   --  Objects
   Root_Obj              : aliased Root;
   Interior_Obj          : aliased Interior;
   InteriorWithEmpty_Obj : aliased InteriorWithEmpty;
   Empty_Obj             : aliased Empty;
begin
   --  One-time setup
   D_Bus.Connection.G_Main.Setup_With_G_Main (Connection);

   --  Create and register objects
   Root_Obj.Create (Connection, +"/");
   Interior_Obj.Create (Connection, +"/Interior");
   InteriorWithEmpty_Obj.Create (Connection, +"/InteriorWithEmpty");
   Empty_Obj.Create (Connection, +"/Empty");

   Register (Root_Obj'Unchecked_Access);
   Register (Interior_Obj'Unchecked_Access);
   Register (InteriorWithEmpty_Obj'Unchecked_Access);
   Register (Empty_Obj'Unchecked_Access);

   --  Main loop
   D_Bus.Connection.Request_Name (Connection, "test.Server");
   Root_Obj.Ready;
   D_Bus.G_Main.Start;
   D_Bus.Connection.Release_Name (Connection, "test.Server");

   --  Clean up
   Interior_Obj.Destroy;
   InteriorWithEmpty_Obj.Destroy;
   Empty_Obj.Destroy;
   Root_Obj.Destroy;
end Server;
