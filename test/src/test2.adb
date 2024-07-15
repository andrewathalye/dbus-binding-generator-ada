with Ada.Text_IO;
with Ada.Strings.Unbounded;

with tk_zenithseeker_Hyprwatch;
with org_freedesktop_DBus_Introspectable;

procedure Test is
   XML : Ada.Strings.Unbounded.Unbounded_String;
begin
   org_freedesktop_DBus_Introspectable.Connect ("tk.zenithseeker.hyprwatch");
   org_freedesktop_DBus_Introspectable.Introspect (XML);
   Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (XML));

   tk_zenithseeker_Hyprwatch.Connect ("tk.zenithseeker.hyprwatch");
   tk_zenithseeker_Hyprwatch.ActivateWorkspace ((3, 1));
end Test;
