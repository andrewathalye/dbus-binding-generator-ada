pragma Ada_2022;

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;            use Interfaces;

with org_freedesktop_DBus;

procedure DBus is
   package OFD renames org_freedesktop_DBus;

   US  : Unbounded_String;
   AS  : OFD.Array_s;
   U32 : Unsigned_32;
   DSV : OFD.Dict_sv;
   AY  : OFD.Array_y;
begin
   OFD.Set_Destination ("org.freedesktop.DBus");
   Put_Line (OFD.Get_Features'Image);
   Put_Line (OFD.Get_Interfaces'Image);

   OFD.ListNames (AS);
   Put_Line (AS'Image);

   OFD.RequestName
     (To_Unbounded_String ("tk.zenithseeker.dbusbinder"), 0, U32);
   Put_Line (U32'Image);

   OFD.ReleaseName (To_Unbounded_String ("org.freedesktop.DBus.B"), U32);
   Put_Line (U32'Image);

   OFD.GetConnectionCredentials
     (To_Unbounded_String ("tk.zenithseeker.dbusbinder"), DSV);
   Put_Line (DSV'Image);

   for E of DSV loop
      Put_Line (E.Get_Argument.To_String'Image);
   end loop;

   OFD.GetAdtAuditSessionData
     (To_Unbounded_String ("tk.zenithseeker.dbusbinder"), AY);
   Put_Line (AY'Image);
end DBus;
