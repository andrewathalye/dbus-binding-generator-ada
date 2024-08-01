pragma Ada_2012;

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  D_Bus
with D_Bus.Connection;
with D_Bus.Types;
use type D_Bus.Types.Obj_Path;
with D_Bus.Support.Server;
with D_Bus.Support.Client;

--  Interfaces
with com_example_Mixed.Client; use com_example_Mixed.Client;
with com_example_Mixed.Server; use com_example_Mixed.Server;

--  Generated
with D_Bus.Generated_Objects; use D_Bus.Generated_Objects;

procedure Bindings_Mixed is
   --  Types
   type Server is
   new D_Bus.Support.Server.Server_Object and
     com_example_Mixed.Server.Child_Interface with null record;

   type Client is
   new D_Bus.Support.Client.Client_Object and
     com_example_Mixed.Client.Child_Interface with null record;

   --  Objects
   Server_Obj : aliased Server;
   Client_Obj : Client;

   Server_Obj_System : aliased Server;
   Client_Obj_System : Client;

   --  Variables
   System_Bus : constant D_Bus.Connection.Connection_Type :=
     D_Bus.Connection.Connect (D_Bus.Bus_System);
begin
   Server_Obj.Create (+"/");
   Client_Obj.Create (+"/");

   Server_Obj_System.Create (+"/", System_Bus);
   Client_Obj_System.Create (+"/", System_Bus);

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

   --  Try destroying a registered object
   Put_Line ("Test destroying registered objects");
   begin
      Server_Obj.Destroy;
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error =>
         null;
   end;

   --  Unregister
   Server_Obj.Unregister;
   Server_Obj_System.Unregister;

   --  Test double-unregistration
   Put_Line ("Test double unregister");
   begin
      Server_Obj_System.Unregister;
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error =>
         null;
   end;

   --  Register Signal Handlers
   Client_Obj.Register_Signal1;
   Client_Obj.Register_Signal2;
   Client_Obj_System.Register_Signal1;
   Client_Obj_System.Register_Signal2;

   --  Test double register
   Put_Line ("Test double signal register");
   begin
      Client_Obj_System.Register_Signal2;
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error =>
         null;
   end;

   --  Send Signals
   --  The idea is to send them on different busses and then
   --  receive them out of order.
   Put_Line ("Test receiving identical signals on different busses");
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

   --  Try destroying an object with registered signals
   Put_Line ("Test destroying an object with registered signals");
   begin
      Client_Obj.Destroy;
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error =>
         null;
   end;

   --  Unregister all signals
   Client_Obj.Unregister_Signal1;
   Client_Obj.Unregister_Signal2;
   Client_Obj_System.Unregister_Signal1;
   Client_Obj_System.Unregister_Signal2;

   --  Test double unregister
   Put_Line ("Test double signal unregister");
   begin
      Client_Obj_System.Unregister_Signal2;
      raise Program_Error;
   exception
      when D_Bus.D_Bus_Error =>
         null;
   end;

   --  Destroy all objects
   Client_Obj.Destroy;
   Client_Obj_System.Destroy;

   Server_Obj.Destroy;
   Server_Obj_System.Destroy;
end Bindings_Mixed;
