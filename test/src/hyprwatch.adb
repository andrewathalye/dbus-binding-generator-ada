pragma Ada_2012;

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with tk_zenithseeker_hyprwatch;
with D_Bus.Support; use D_Bus.Support;

procedure Hyprwatch is
   type TZ_Hyprwatch is
   new Root_Object and tk_zenithseeker_hyprwatch.Child_Interface with
   null record;

   TZH  : TZ_Hyprwatch;
   json : Unbounded_String;
begin
   TZH.Create (To_Unbounded_String ("/tk/zenithseeker/hyprwatch"));
   TZH.Set_Destination ("tk.zenithseeker.hyprwatch");

   TZH.Register_HyprUpdate;
   loop
      TZH.Await_HyprUpdate (json);
      Put_Line (To_String (json));
   end loop;
end Hyprwatch;
