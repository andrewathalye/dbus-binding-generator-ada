pragma Ada_2012;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  D_Bus
with D_Bus.Arguments.Basic; use D_Bus.Arguments.Basic;
with D_Bus.Arguments.Containers; use D_Bus.Arguments.Containers;
with D_Bus.Types; use D_Bus.Types;

--  Support
with D_Bus.Support.Client;

--  Interfaces
with org_freedesktop_DBus_Properties.Client;
with com_example_Control.Client;
with com_example_Properties.Client;

--  Generated
with D_Bus.Generated_Types; use D_Bus.Generated_Types;

with Shared; use Shared;

procedure Properties_Client (Connection : D_Bus.Connection.Connection_Type) is
   --  Object Types
   type Properties is new D_Bus.Support.Client.Client_Object
      and org_freedesktop_DBus_Properties.Client.Child_Interface
      and com_example_Control.Client.Child_Interface
      and com_example_Properties.Client.Child_Interface
      with null record;

   --  Objects
   O : aliased Properties;

   --  Variables
   Buf : Unbounded_String;
   Variant_String : Variant_Type := Create (+"Hello");
   Variant_Int : constant Variant_Type := Create (+D_Bus.Signed_32'(1));
   All_Props : Dict_sv;
begin
   O.Create (Connection, +"/Properties");
   O.Register;
   O.Set_Destination ("test.Properties");

   --  Wait for server to go up
   begin
      O.Ping;
   exception
      when D_Bus.D_Bus_Error =>
         O.Await_Ready;
   end;

   --  Correct usage using bindings
   Buf := O.TestPropertyReadOnly;
   O.Set_TestPropertyWriteOnly (Buf);
   Buf := O.TestProperty;
   O.Set_TestProperty (Buf);

   --  Raw interface
   Put_Line ("Get invalid iface");
   begin
      O.Set_Property
        ("invalid.Interface", "InvalidProperty", Variant_String);
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error => null;
   end;

   Put_Line ("Set invalid iface");
   begin
      O.Get_Property
        ("invalid.Interface", "InvalidProperty", Variant_String);
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error => null;
   end;

   Put_Line ("Get all invalid iface (not erroneous)");
   O.GetAll
     (+"invalid.Interface", All_Props);

   Put_Line ("Get all");
   O.GetAll
     (+"com.example.Properties", All_Props);

   Put_Line ("Set invalid");
   begin
      O.Set_Property
        ("com.example.Properties", "InvalidProperty", Variant_String);
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error => null;
   end;

   Put_Line ("Get invalid");
   begin
      O.Get_Property
        ("com.example.Properties", "InvalidProperty", Variant_String);
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error => null;
   end;

   Put_Line ("Set readonly");
   begin
      O.Set_Property
        ("com.example.Properties",
         "TestPropertyReadOnly", Variant_String);
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error => null;
   end;

   Put_Line ("Get writeonly");
   begin
      O.Get_Property
        ("com.example.Properties",
         "TestPropertyWriteOnly", Variant_String);
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error => null;
   end;

   Put_Line ("Set with invalid variant type");
   begin
      O.Set_Property
        ("com.example.Properties", "TestPropertyWriteOnly", Variant_Int);
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error => null;
   end;

   --  Tell the server to close
   O.Quit;

   O.Destroy;
end Properties_Client;
