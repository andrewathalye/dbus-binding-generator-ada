pragma Ada_2012;

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings;

--  D_Bus
with D_Bus.Types;
use type D_Bus.Types.Obj_Path;
with D_Bus.Support.Server;
with D_Bus.Support.Client;
with D_Bus.Messages;
with dbus_message_h;

--  Interfaces
with com_example_Mixed.Client; use com_example_Mixed.Client;
with com_example_Mixed.Server; use com_example_Mixed.Server;

--  Generated
with D_Bus.Generated_Objects; use D_Bus.Generated_Objects;

procedure Mixed (Session_Bus : D_Bus.Connection.Connection_Type) is
   --  Types
   type Server is
   new D_Bus.Support.Server.Server_Object and
     com_example_Mixed.Server.Child_Interface with null record;

   type Client is
   new D_Bus.Support.Client.Client_Object and
     com_example_Mixed.Client.Child_Interface with null record;

   --  Objects
   Server_Obj : aliased Server;
   Client_Obj : aliased Client;

   Server_Obj_System : aliased Server;
   Client_Obj_System : aliased Client;

   --  Variables
   System_Bus : constant D_Bus.Connection.Connection_Type :=
     D_Bus.Connection.Connect (D_Bus.Bus_System);
begin
   Server_Obj.Create (Session_Bus, +"/Mixed");
   Client_Obj.Create (Session_Bus, +"/Mixed");

   Server_Obj_System.Create (System_Bus, +"/Mixed");
   Client_Obj_System.Create (System_Bus, +"/Mixed");

   --  Register
   Register (Server_Obj'Unchecked_Access);
   Register (Server_Obj_System'Unchecked_Access);

   --  Test double-registration
   Put_Line ("Test double register");
   begin
      Register (Server_Obj_System'Unchecked_Access); --  Erroneous
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error =>
         null;
   end;

   --  Send Signals
   --  The idea is to send them on different busses and then
   --  receive them out of order.
   Put_Line ("Test receiving identical signals on different busses");
   Client_Obj.Register;
   Client_Obj_System.Register;
   Server_Obj.Signal1 (To_Unbounded_String ("Session Bus"));
   Server_Obj.Signal2;
   Server_Obj_System.Signal1 (To_Unbounded_String ("System Bus"));
   Server_Obj_System.Signal2;

   Client_Obj.Await_Signal2;
   Client_Obj_System.Await_Signal2;

   declare
      Msg        : Unbounded_String;
      Msg_System : Unbounded_String;
   begin
      Client_Obj_System.Await_Signal1 (Msg_System);
      Client_Obj.Await_Signal1 (Msg);

      if To_String (Msg) = "Session Bus" and
        To_String (Msg_System) = "System Bus"
      then
         null;
      else
         raise Program_Error;
      end if;
   end;

   --  Send non-signal to client handler
   --  This is a realistic scenario in a a mixed-mode
   --  single-threaded application. The client could receive
   --  a message intended for the server while waiting for
   --  a registered signal.

   Put_Line ("Send non-signal to client handler");
   declare
      use Interfaces.C.Strings;

      use D_Bus.Messages;
      use D_Bus.Connection;
      use dbus_message_h;

      bus_name : chars_ptr := New_String ("test.Mixed");
      path : chars_ptr := New_String ("/Mixed");
      method : chars_ptr := New_String ("test");

      Message : Message_Type;
   begin
      --  Create semantically invalid method call
      Message := Create
        (dbus_message_new_method_call
           (bus_name => bus_name,
            path     => path,
            iface    => Null_Ptr,
            method   => method));

      Free (bus_name);
      Free (path);
      Free (method);

      --  Push call to the client
      Request_Name (Session_Bus, "test.Mixed");
      Send (Session_Bus, Message);
      Release_Name (Session_Bus, "test.Mixed");
      Unref (Message);

      --  Use Signal2 to trigger the callback.
      Server_Obj.Signal2;
      Client_Obj.Await_Signal2;
   end;

   --  Destroy all objects
   Client_Obj.Destroy;
   Client_Obj_System.Destroy;

   Server_Obj.Destroy;
   Server_Obj_System.Destroy;

   D_Bus.Connection.Unref (System_Bus);
end Mixed;
