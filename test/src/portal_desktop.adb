pragma Ada_2022;

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces;
with GNAT.OS_Lib;

with org_freedesktop_portal_FileChooser;
with org_freedesktop_portal_Print;
with org_freedesktop_portal_Request;

with D_Bus.Support;         use D_Bus.Support;
with D_Bus.Generated_Types; use D_Bus.Generated_Types;
with D_Bus.Arguments.Containers;

procedure Portal_Desktop is
   --  org.freedesktop.portal.FileChooser
   type OFDP_FileChooser is
   new D_Bus.Support.Root_Object and
     org_freedesktop_portal_FileChooser.Child_Interface and
     org_freedesktop_portal_Print.Child_Interface with null record;

   --  org.freedesktop.portal.Request
   type OFDP_Request is
   new D_Bus.Support.Root_Object and
     org_freedesktop_portal_Request.Child_Interface with null record;

   --  /org/freedesktop/portal/desktop
   Desktop : OFDP_FileChooser;
begin
   Put_Line ("/org/freedesktop/portal/desktop");
   Desktop.Create (To_Unbounded_String ("/org/freedesktop/portal/desktop"));
   Desktop.Set_Destination ("org.freedesktop.portal.Desktop");

   Put_Line ("OpenFile");
   --  OpenFile
   declare
      OpenFile_Handle : Unbounded_Object_Path;
      OpenFile_Object : OFDP_Request;

      Response : Interfaces.Unsigned_32;
      Results  : Dict_sv;
   begin
      Desktop.OpenFile
        (parent_window => Null_Unbounded_String,
         title         => To_Unbounded_String ("Sample select dialogue"),
         options       => [], handle => OpenFile_Handle);

      Put_Line (To_String (OpenFile_Handle));
      Put_Line ("@Request");
      --  Retrieve the response signal
      OpenFile_Object.Create (OpenFile_Handle);
      OpenFile_Object.Register_Response;
      OpenFile_Object.Await_Response (Response, Results);
      OpenFile_Object.Unregister_Response;
      OpenFile_Object.Destroy;

      Put_Line (Results'Image);
      for Result of Results loop
         Put_Line (D_Bus.Arguments.Containers.To_String (Result));
      end loop;
   end;

   Put_Line ("Print");
   --  Print
   declare
      File         : GNAT.OS_Lib.File_Descriptor;
      Print_Handle : Unbounded_Object_Path;
      Print_Object : OFDP_Request;

      Response : Interfaces.Unsigned_32;
      Results  : Dict_sv;
   begin
      File :=
        GNAT.OS_Lib.Open_Read ("src/portal_desktop.adb", GNAT.OS_Lib.Text);

      Desktop.Print
        (parent_window => Null_Unbounded_String,
         title => To_Unbounded_String ("Sample print dialogue"), fd => File,
         options       => [], handle => Print_Handle);

      GNAT.OS_Lib.Close (File);

      Put_Line (To_String (Print_Handle));
      Put_Line ("@Request");
      --  Retrieve the response signal
      Print_Object.Create (Print_Handle);
      Print_Object.Register_Response;
      Print_Object.Await_Response (Response, Results);
      Print_Object.Unregister_Response;
      Print_Object.Destroy;

      Put_Line (Results'Image);
      for Result of Results loop
         Put_Line (D_Bus.Arguments.Containers.To_String (Result));
      end loop;
   end;
end Portal_Desktop;
