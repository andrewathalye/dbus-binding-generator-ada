with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with tk_zenithseeker_hyprwatch_org_freedesktop_DBus_Introspectable;
with tk_zenithseeker_hyprwatch_tk_zenithseeker_hyprwatch;

procedure Hyprwatch is
   package OFDI renames
     tk_zenithseeker_hyprwatch_org_freedesktop_DBus_Introspectable;
   package TZH renames tk_zenithseeker_hyprwatch_tk_zenithseeker_hyprwatch;

   XML  : Unbounded_String;
   json : Unbounded_String;
begin
   OFDI.Set_Destination ("tk.zenithseeker.hyprwatch");
   OFDI.Introspect (XML);
   Put_Line (To_String (XML));

   TZH.Set_Destination ("tk.zenithseeker.hyprwatch");
   TZH.Await_HyprUpdate (json);
   Put_Line (To_String (json));
end Hyprwatch;
