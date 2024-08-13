pragma Ada_2012;

with Ada.Strings.Unbounded;
with D_Bus.Arguments.Containers; use D_Bus.Arguments.Containers;

--  D_Bus
with D_Bus.Connection.G_Main;
with D_Bus.G_Main;
with D_Bus.Types; use D_Bus.Types;

--  Support
with D_Bus.Support.Server; use D_Bus.Support.Server;

--  Interfaces
with com_example_Control.Server;
with com_example_Properties.Server;

--  Generated
with D_Bus.Generated_Objects;
with D_Bus.Generated_Types; use D_Bus.Generated_Types;

with Shared; use Shared;

procedure Properties_Server (Connection : D_Bus.Connection.Connection_Type) is
   --  Object Types
   type Properties is new Server_Object
      and com_example_Control.Server.Child_Interface
      and com_example_Properties.Server.Child_Interface
      with null record;

   --  Specifications
   procedure Create
     (O : out Properties;
      Connection : D_Bus.Connection.Connection_Type;
      Node : D_Bus.Types.Obj_Path);
   overriding procedure Quit (O : in out Properties);

   --  Implementation
   procedure Create
     (O : out Properties;
      Connection : D_Bus.Connection.Connection_Type;
      Node : D_Bus.Types.Obj_Path)
   is
   begin
      Server_Object (O).Create (Connection, Node);
      O.Set_TestPropertyReadOnly (+"ReadOnly");
      O.Set_TestPropertyWriteOnly (+"WriteOnly");
      O.Set_TestProperty (+"Readwrite");
   end Create;

   procedure Quit (O : in out Properties) is
   begin
      D_Bus.G_Main.Quit;
   end Quit;

   --  Objects
   O : aliased Properties;
begin
   D_Bus.Connection.G_Main.Setup_With_G_Main (Connection);

   O.Create (Connection, +"/Properties");
   D_Bus.Generated_Objects.Register (O'Unchecked_Access);

   D_Bus.Connection.Request_Name (Connection, "test.Properties");
   O.Ready;
   D_Bus.G_Main.Start;
   D_Bus.Connection.Release_Name (Connection, "test.Properties");

   O.Destroy;
end Properties_Server;
