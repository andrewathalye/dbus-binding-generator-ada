pragma Ada_2012;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;
with GNAT.OS_Lib;

--  D_Bus
with D_Bus.Arguments.Containers;
with D_Bus.Arguments.Basic;
with D_Bus.Types; use D_Bus.Types;

--  Support
with D_Bus.Support.Server; use D_Bus.Support.Server;

--  Interfaces
with tk_zenithseeker_Control.Server;
with com_example_Duplicate.Server;
with com_example_Interface.Server;
with com_example_Interior.Server;
with com_example_InteriorWithEmpty.Server;

--  Generated
with D_Bus.Generated_Objects; use D_Bus.Generated_Objects;
with D_Bus.Generated_Types;   use D_Bus.Generated_Types;

procedure Bindings_Server is
   --  Renamings
   function "+" (L : String) return Unbounded_String renames
     To_Unbounded_String;

   --  Types
   type Root is
   new D_Bus.Support.Server.Server_Object and
     tk_zenithseeker_Control.Server.Child_Interface and
     com_example_Duplicate.Server.Child_Interface and
     com_example_Interface.Server.Child_Interface with record
      Should_Quit : Boolean := False;
   end record;

   type Interior is
   new D_Bus.Support.Server.Server_Object and
     com_example_Interior.Server.Child_Interface with null record;

   type InteriorWithEmpty is
   new D_Bus.Support.Server.Server_Object and
     com_example_InteriorWithEmpty.Server.Child_Interface with null record;

   type Empty is new D_Bus.Support.Server.Server_Object with null record;

   --  Override Specs
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

   overriding procedure TestComplexTypesOut
     (O : in out Root; simple_struct : out Struct_s; simple_arr : out Array_s;
      int_dict      :    out Dict_iv; string_dict : out Dict_sv;
      obj_path_dict :    out Dict_ov; signature_dict : out Dict_gv;
      nested_dict   :    out Dict_saesve; nested_struct : out Struct_rsr;
      novel_array   :    out Array_rhhhhhr) is null;

   overriding procedure Quit (O : in out Root);

   --  Override Implementation
   pragma Warnings (Off, "-gnatwf");
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
      Parameter_11 := +"/";
      Parameter_13 := D_Bus.Arguments.Containers.Create (+"Variant");
   end TestBasicTypesOut;
   pragma Warnings (On);

   overriding procedure Quit (O : in out Root) is
   begin
      O.Should_Quit := True;
      O.QuitSignal (+"Exiting now!");
   end Quit;

   --  Objects
   Root_Obj              : aliased Root;
   Interior_Obj          : aliased Interior;
   InteriorWithEmpty_Obj : aliased InteriorWithEmpty;
   Empty_Obj             : aliased Empty;
begin
   --  One-time setup
   D_Bus.Support.Server.Setup_With_G_Main;

   --  Create and register objects
   Root_Obj.Create (+"/");
   Interior_Obj.Create (+"/Interior");
   InteriorWithEmpty_Obj.Create (+"/InteriorWithEmpty");
   Empty_Obj.Create (+"/Empty");

   Register (Root_Obj'Unchecked_Access);
   Register (Interior_Obj'Unchecked_Access);
   Register (InteriorWithEmpty_Obj'Unchecked_Access);
   Register (Empty_Obj'Unchecked_Access);

   --  Set specified properties
   Root_Obj.Set_TestPropertyReadOnly (+"Read Only");
   Root_Obj.Set_TestPropertyWriteOnly (+"Write Only");
   Root_Obj.Set_TestProperty (+"Read Write");

   --  Main loop
   Request_Name ("test.Service");
   while not Root_Obj.Should_Quit loop
      D_Bus.Support.Server.Run_Iteration;
   end loop;
   Release_Name ("test.Service");

   --  Clean up
   Interior_Obj.Unregister;
   InteriorWithEmpty_Obj.Unregister;
   Empty_Obj.Unregister;
   Root_Obj.Unregister;

   Interior_Obj.Destroy;
   InteriorWithEmpty_Obj.Destroy;
   Empty_Obj.Destroy;
   Root_Obj.Destroy;
end Bindings_Server;
