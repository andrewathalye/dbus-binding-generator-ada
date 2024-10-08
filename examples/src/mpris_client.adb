pragma Ada_2012;

--  Standard Library
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

--  D_Bus/Ada
with D_Bus.Types;
with D_Bus.Connection;
use type D_Bus.Types.Obj_Path;

--  D_Bus Interfaces
with org_freedesktop_DBus.Client;
with org_mpris_MediaPlayer2.Client;
with org_mpris_MediaPlayer2_Player.Client;
with org_mpris_MediaPlayer2_Playlists.Client;
with org_mpris_MediaPlayer2_TrackList.Client;

--  Support Code
with D_Bus.Support.Client;  use D_Bus.Support.Client;
with D_Bus.Generated_Types; use D_Bus.Generated_Types;

procedure MPRIS_Client is
   --  D_Bus Object Types
   type D_Bus_Object is
   new Client_Object and org_freedesktop_DBus.Client.Child_Interface with
   null record;

   type MPRIS_Object is
   new Client_Object and org_mpris_MediaPlayer2.Client.Child_Interface and
     org_mpris_MediaPlayer2_Player.Client.Child_Interface and
     org_mpris_MediaPlayer2_Playlists.Client.Child_Interface and
     org_mpris_MediaPlayer2_TrackList.Client.Child_Interface with null record;

   --  Renamings
   function "+" (Item : Unbounded_String) return String renames To_String;
   function "+" (Item : String) return Unbounded_String renames
     To_Unbounded_String;

   --  D_Bus Objects
   Bus    : D_Bus_Object;
   Player : MPRIS_Object;

   --  Variables
   Connection : D_Bus.Connection.Connection_Type := D_Bus.Connection.Connect;
   Prefix    : constant Unbounded_String := +"org.mpris.MediaPlayer2";
   Name_List : Array_s;
begin
   --  Constructors
   Bus.Create (Connection, +"/org/freedesktop/DBus");
   Bus.Set_Destination ("org.freedesktop.DBus");
   Player.Create (Connection, +"/org/mpris/MediaPlayer2");

   --  Find Players
   Bus.ListNames (Name_List);

   Get_Names :
   for Name of Name_List loop
      --  If the name is prefixed "org.mpris.MediaPlayer2"
      if Head (Name, Length (Prefix)) = Prefix then
         Player.Set_Destination (+Name);

         --  Print out bus name and player name
         Put_Line (+Name);
         Put_Line (+Player.Identity);

         --  Print out metadata
         Print_Metadata :
         declare
            use Pkg_Dict_sv;
         begin
            for Cursor in Player.Metadata.Iterate loop
               Put ((+Key (Cursor)) & ": ");
               Put_Line (Element (Cursor).Get_Argument.To_String);
            end loop;
            New_Line;
         end Print_Metadata;
      end if;
   end loop Get_Names;

   --  Destructors
   Player.Destroy;
   Bus.Destroy;
   D_Bus.Connection.Free (Connection);
end MPRIS_Client;
