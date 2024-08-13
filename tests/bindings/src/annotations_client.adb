pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  D_Bus
with D_Bus.Support.Client;
with D_Bus.Types;
use type D_Bus.Types.Obj_Path;

--  Generated
with D_Bus.Generated_Types; use D_Bus.Generated_Types;

--  Interfaces
with com_example_Control.Client;
with com_example_Annotations.Client;
with com_example_Annotations_Deprecated.Client;
with org_freedesktop_DBus_Properties.Client;

with Shared; use Shared;

procedure Annotations_Client (Connection : D_Bus.Connection.Connection_Type)
is
   type Annotations_Object is new D_Bus.Support.Client.Client_Object
      and org_freedesktop_DBus_Properties.Client.Child_Interface
      and com_example_Control.Client.Child_Interface
      and com_example_Annotations.Client.Child_Interface with null record;

   O : aliased Annotations_Object;
begin
   O.Create (Connection, +"/Annotations");
   O.Set_Destination ("test.Annotations");
   O.Register;

   --  Wait for server to be ready
   --  If Ping goes through then the server is already up.
   begin
      O.Ping;
   exception
      when D_Bus.D_Bus_Error =>
         O.Await_Signal;
   end;
   O.Purge_Signal;

   Put_Line ("Call method marked NoReply");
   O.Method;

   Put_Line ("Read deprecated property");

   --  Deprecated property
   declare
      Discard : Unbounded_String;
   begin
      Discard := O.Deprecated;
   end;

   --  Set Properties and Inspect PropertiesChanged
   O.Purge_PropertiesChanged;
   O.Set_False (+"False");
   O.Set_Const (+"Const");
   O.Set_True (+"True");
   O.Set_Invalidates (+"Invalidates");

   declare
      Iface : Unbounded_String;
      Values : Dict_sv;
      Invalidated : Array_s;
   begin
      --  Should be "True"
      O.Await_PropertiesChanged (Iface, Values, Invalidated);
      if not Values.Contains (+"True") then
         raise Program_Error with "Erroneous signal data";
      end if;

      --  Should be "Invalidates" with no value
      O.Await_PropertiesChanged (Iface, Values, Invalidated);
      if not Invalidated.Contains (+"Invalidates")
      then
         raise Program_Error with "Erroneous signal data";
      end if;
   end;

   Put_Line ("Quit");
   O.Quit;
   O.Destroy;
end Annotations_Client;
