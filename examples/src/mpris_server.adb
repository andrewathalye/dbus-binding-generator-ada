pragma Ada_2012;

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib;

--  D_Bus Library
with D_Bus.Arguments.Basic;
with D_Bus.Arguments.Containers;
with D_Bus.Types;
use type D_Bus.Types.Obj_Path;
with D_Bus.Support.Server; use D_Bus.Support.Server;

--  Interfaces
with org_mpris_MediaPlayer2;
with org_mpris_MediaPlayer2_Player;

--  Generated Packages
with D_Bus.Generated_Objects;
with D_Bus.Generated_Types; use D_Bus.Generated_Types;

procedure MPRIS_Server is
   ---------------
   -- Renamings --
   ---------------
   function "+" (Item : Unbounded_String) return String renames To_String;
   function "+" (Item : String) return Unbounded_String renames
      To_Unbounded_String;

   ------------------
   -- MPRIS_Object --
   ------------------
   type MPRIS_Object is new Server_Object
      and org_mpris_MediaPlayer2.Child_Interface
      and org_mpris_MediaPlayer2_Player.Child_Interface with
   record
      Should_Quit : Boolean := False;
   end record;

   overriding procedure Create
     (O : out MPRIS_Object; Node : D_Bus.Types.Obj_Path);
   overriding procedure Quit (O : in out MPRIS_Object);
   overriding procedure Play (O : in out MPRIS_Object);
   overriding procedure Pause (O : in out MPRIS_Object);
   overriding procedure PlayPause (O : in out MPRIS_Object);

   -----------------------------
   -- Methods on MPRIS_Object --
   -----------------------------
   overriding procedure Create
     (O : out MPRIS_Object; Node : D_Bus.Types.Obj_Path)
   is
   begin
      --  Create base object
      Server_Object (O).Create (Node);

      --  Set properties for org.mpris.MediaPlayer2
      O.Set_CanQuit (True);
      O.Set_CanRaise (False);
      O.Set_HasTrackList (False);
      O.Set_Identity (+"TestPlayer");
      O.Set_SupportedUriSchemes (Pkg_Array_s.Empty_Vector);
      O.Set_SupportedMimeTypes (Pkg_Array_s.Empty_Vector);

      --  Set properties for org.mpris.MediaPlayer2.Player
      O.Set_PlaybackStatus (+"Paused");
      O.Set_Rate (1.0);
      O.Set_Metadata (Pkg_Dict_sv.Empty_Map);
      O.Set_Volume (1.0);
      O.Set_Position (0);
      O.Set_MinimumRate (1.0);
      O.Set_MaximumRate (1.0);
      O.Set_CanGoNext (False);
      O.Set_CanGoPrevious (False);
      O.Set_CanPlay (True);
      O.Set_CanPause (True);
      O.Set_CanSeek (False);
      O.Set_CanControl (True);
   end Create;

   overriding procedure Quit (O : in out MPRIS_Object)
   is
   begin
      Put_Line ("Quit");
      O.Should_Quit := True;
   end Quit;

   overriding procedure Play (O : in out MPRIS_Object)
   is
   begin
      Put_Line ("Play");

      O.Set_PlaybackStatus (+"Playing");
   end Play;

   overriding procedure Pause (O : in out MPRIS_Object)
   is
   begin
      Put_Line ("Pause");

      O.Set_PlaybackStatus (+"Paused");
   end Pause;

   overriding procedure PlayPause (O : in out MPRIS_Object)
   is
   begin
      Put_Line ("PlayPause");

      if +O.PlaybackStatus = "Paused" then
         O.Set_PlaybackStatus (+"Playing");
      else
         O.Set_PlaybackStatus (+"Paused");
      end if;
   end PlayPause;

   --  Variables
   Server : aliased MPRIS_Object;
begin
   --  Initial setup
   Put_Line ("Set up server");
   D_Bus.Support.Server.Setup_With_G_Main;

   --  Object
   Put_Line ("Create and register object");
   Server.Create (+"/org/mpris/MediaPlayer2");
   D_Bus.Generated_Objects.Register (Server'Unchecked_Access);

   --  Update properties
   Put_Line ("Update metadata");
   declare
      use D_Bus.Arguments.Basic;
      use D_Bus.Arguments.Containers;

      Metadata : Dict_sv;
   begin
      Metadata.Insert (+"mpris_trackid", Create (+"/org/mpd/Tracks/1"));
      Metadata.Insert (+"xesam:title", Create (+"Test Track"));
      Server.Set_Metadata (Metadata);
   end;

   --  Register name and start loop
   Put_Line ("Start loop");
   D_Bus.Support.Server.Request_Name ("org.mpris.MediaPlayer2.test");
   while not Server.Should_Quit loop
      D_Bus.Support.Server.Run_Iteration;
   end loop;

   --  Cleanup
   Put_Line ("Cleanup");
   D_Bus.Support.Server.Release_Name ("org.mpris.MediaPlayer2.test");
   Server.Unregister;
   Server.Destroy;
end MPRIS_Server;
