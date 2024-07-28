pragma Ada_2012;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with D_Bus.Arguments.Basic;
with D_Bus.Arguments.Containers;
with D_Bus.Support;
with D_Bus.Support.Server; use D_Bus.Support.Server;

with org_mpris_MediaPlayer2;
with org_mpris_MediaPlayer2_Player;

with D_Bus.Generated_Dispatcher;

procedure MPRIS_Server is
   type MPRIS_Object is new Server_Object
      and org_mpris_MediaPlayer2.Child_Interface
      and org_mpris_MediaPlayer2_Player.Child_Interface with null record;

   Server_Node : constant D_Bus.Support.Unbounded_Object_Path :=
     To_Unbounded_String ("/org/mpris/MediaPlayer2");
   Server : MPRIS_Object;
begin
   D_Bus.Support.Server.Request_Name ("org.mpris.MediaPlayer2.test");
   D_Bus.Generated_Dispatcher.Add_Object (Server, Server_Node);
   D_Bus.Generated_Dispatcher.Blocking_Dispatch;
   D_Bus.Generated_Dispatcher.Remove_Object (Server_Node);
end MPRIS_Server;
