pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;
with D_Bus.Connection.G_Main;
with D_Bus.Types;
with D_Bus.G_Main;
use type D_Bus.Types.Obj_Path;

with D_Bus.Support.Server;

with com_example_Annotations.Server;
with D_Bus.Generated_Objects;

with Shared; use Shared;

procedure Annotations_Server (Connection : D_Bus.Connection.Connection_Type) is
   --  Object Type Implementation
   type Server_Object is new D_Bus.Support.Server.Server_Object
      and com_example_Annotations.Server.Child_Interface
      with null record;

   overriding procedure Quit (O : in out Server_Object);
   overriding procedure Quit (O : in out Server_Object) is
   begin
      D_Bus.G_Main.Quit;
   end Quit;

   --  Objects
   Object : aliased Server_Object;
begin
   D_Bus.Connection.G_Main.Setup_With_G_Main (Connection);

   --  Create objects
   Object.Create (Connection, +"/");
   D_Bus.Generated_Objects.Register (Object'Unchecked_Access);

   --  Set properties
   Object.Set_Deprecated (+"Deprecated");
   Object.Set_True (+"True");
   Object.Set_Invalidates (+"Invalidates");
   Object.Set_Const (+"Const");
   Object.Set_False (+"False");

   --  Start service
   Put_Line ("SERVER: Start Service");
   D_Bus.Connection.Request_Name (Connection, "test.Service");
   Object.Signal;
   D_Bus.G_Main.Start;
   D_Bus.Connection.Release_Name (Connection, "test.Service");
   Put_Line ("SERVER: End Service");

   --  Clean up
   Object.Unregister;
   Object.Destroy;
end Annotations_Server;
