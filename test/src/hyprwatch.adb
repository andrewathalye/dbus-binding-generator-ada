with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with tk_zenithseeker_hyprwatch.tk_zenithseeker_hyprwatch;
use tk_zenithseeker_hyprwatch.tk_zenithseeker_hyprwatch;

procedure Hyprwatch is
   package TZH renames tk_zenithseeker_hyprwatch.tk_zenithseeker_hyprwatch;

   json : Unbounded_String;
begin
   tk_zenithseeker_hyprwatch.Set_Destination ("tk.zenithseeker.hyprwatch");

   TZH.Register_HyprUpdate;
   loop
      TZH.Await_HyprUpdate (json);
      Put_Line (To_String (json));
   end loop;
end Hyprwatch;
