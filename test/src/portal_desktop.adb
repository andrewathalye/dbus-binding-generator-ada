pragma Ada_2022;

with Ada.Text_IO;
with Interfaces;
with org_freedesktop_portal_desktop.org_freedesktop_portal_FileChooser;
use org_freedesktop_portal_desktop.org_freedesktop_portal_FileChooser;

with Root.org_freedesktop_portal_Request;
use Root.org_freedesktop_portal_Request;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with D_Bus.Support;         use D_Bus.Support;
with D_Bus.Generated_Types; use D_Bus.Generated_Types;
with D_Bus.Arguments.Containers;
with D_Bus.Arguments.Basic;
use type D_Bus.Arguments.Basic.Boolean_Type;

procedure Portal_Desktop is
   Out_Handle : Unbounded_Object_Path;
   V          : D_Bus.Arguments.Containers.Variant_Type;
   Options    : Dict_sv;
begin
   pragma Warnings (Off);
   org_freedesktop_portal_desktop.Set_Destination
     ("org.freedesktop.portal.Desktop");

   V := D_Bus.Arguments.Containers.Create (+False);
   Options.Insert (To_Unbounded_String ("modal"), V);

   OpenFile
     (parent_window => Null_Unbounded_String,
      title => To_Unbounded_String ("Test binding open file"), options => [],
      handle        => Out_Handle);

   --  Get response
   Register_Response (Out_Handle);

   declare
      Response : Interfaces.Unsigned_32;
      Results : Dict_sv;
   begin
      Await_Response (Response, Results);
      Ada.Text_IO.Put_Line (Response'Image);
      Ada.Text_IO.Put_Line (Results'Image);

      for V of Results loop
         Ada.Text_IO.Put_Line
           (D_Bus.Arguments.Containers.To_String (V));
      end loop;
   end;

   Unregister_Response;

end Portal_Desktop;
