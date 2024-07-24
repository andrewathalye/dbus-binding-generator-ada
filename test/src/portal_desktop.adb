pragma Ada_2022;

with Ada.Text_IO;
with D_Bus.Messages;
with Interfaces;
with org_freedesktop_portal_FileChooser;
use org_freedesktop_portal_FileChooser;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with D_Bus.Support;         use D_Bus.Support;
with D_Bus.Generated_Types; use D_Bus.Generated_Types;
with D_Bus.Arguments.Containers;
with D_Bus.Arguments.Basic;
use type D_Bus.Arguments.Basic.Boolean_Type;

with D_Bus.Generated_Objects; use D_Bus.Generated_Objects;

procedure Portal_Desktop is
   Out_Handle : Unbounded_Object_Path;
   V          : D_Bus.Arguments.Containers.Variant_Type;
   Options    : Dict_sv;
begin
   pragma Warnings (Off);

   V := D_Bus.Arguments.Containers.Create (+False);
   Options.Insert (To_Unbounded_String ("modal"), V);

   org_freedesktop_portal_desktop.Set_Destination
     ("org.freedesktop.portal.Desktop");

   org_freedesktop_portal_desktop.OpenFile
     (parent_window => Null_Unbounded_String,
      title => To_Unbounded_String ("Test binding open file"), options => [],
      handle        => Out_Handle);
end Portal_Desktop;
