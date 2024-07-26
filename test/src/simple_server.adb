pragma Ada_2012;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with D_Bus.Arguments.Basic;
with D_Bus.Arguments.Containers;
with D_Bus.Support.Server; use D_Bus.Support.Server;

procedure Simple_Server is
   use type D_Bus.Arguments.Basic.String_Type;

   Server : Server_Object;

   V : D_Bus.Arguments.Containers.Variant_Type;
begin
   D_Bus.Support.Server.Request_Name ("org.test.server");
   Server.Create (To_Unbounded_String ("/org/test/server"));

   V := D_Bus.Arguments.Containers.Create (+"Test");
   Server.Set_Property ("org.test.server", "SimpleProperty", V);

   Server.Destroy;
end Simple_Server;
