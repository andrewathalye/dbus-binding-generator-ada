pragma Ada_2012;

--  Standard Library
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

--  D_Bus Interfaces
with org_mpris_MediaPlayer2_Player;

--  Support Code
with D_Bus.Support;         use D_Bus.Support;
with D_Bus.Generated_Types; use D_Bus.Generated_Types;

procedure MPRIS_Simple is
   --  D_Bus Object Types
   type MPRIS_Object is
   new Root_Object and org_mpris_MediaPlayer2_Player.Child_Interface with
   null record;

   --  Renamings
   function "+" (Item : Unbounded_String) return String renames To_String;
   function "+" (Item : String) return Unbounded_String renames
     To_Unbounded_String;

   --  D_Bus Objects
   Player : MPRIS_Object;
begin
   --  Constructors
   Player.Create (+"/org/mpris/MediaPlayer2");
   Player.Set_Destination ("org.mpris.MediaPlayer2.playerctld");

   --  Print out metadata
   Print_Metadata :
   declare
      use Pkg_Dict_sv;
   begin
      for Cursor in Player.Metadata.Iterate loop
         Put ((+Key (Cursor)) & ASCII.HT);
         Put_Line (Element (Cursor).Get_Argument.To_String);
      end loop;
   end Print_Metadata;

   --  Destructors
   Player.Destroy;
end MPRIS_Simple;
