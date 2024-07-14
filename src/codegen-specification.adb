with Shared; use Shared;

package body Codegen.Specification is
   -----------
   -- Print --
   -----------
   procedure Print
     (Pkg : Ada_Package_Type;
      File : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      procedure Print_Preamble;
      procedure Print_Preamble is
      begin
         Put_Line (File, "pragma Ada_2012;");
         Put_Line (File, "with Ada.Strings.Unbounded;");
         Put_Line (File, "with Ada.Containers.Vectors;");
         Put_Line (File, "with Ada.Containers.Hashed_Maps;");
         Put_Line (File, "with Interfaces;");
         Put_Line (File, "with GNAT.OS_Lib;");
         Put_Line (File, "with D_Bus.Arguments.Containers;");
      end Print_Preamble;

      procedure Print_Types_Builtin;
      procedure Print_Types_Builtin is
      begin
         Put_Line (File, "-------------------------");
         Put_Line (File, "-- Builtin D_Bus Types --");
         Put_Line (File, "-------------------------");
         Put_Line (File, "type Object_Path is new String;");
         Put_Line (File, "type Signature_Type is new String;");
      end Print_Types_Builtin;
   begin
      Print_Preamble;
      New_Line (File);

      Put_Line (File, "package " & (+Pkg.Name) & " is");

      --  Print type declarations
      --  TODO: they’re in the wrong order right now
      Print_Types_Builtin;
      New_Line (File);

      Put_Line (File, "---------------------------");
      Put_Line (File, "-- Generated D_Bus Types --");
      Put_Line (File, "---------------------------");
      for TD of Pkg.Type_Declarations loop
         case TD.Kind is
            when Builtin_Kind => null;
            when Array_Kind =>
               Put_Line
                 (File,
                  "package Pkg_" &
                  (+TD.Name) &
                  " is new Ada.Containers.Vectors (Positive, " &
                  (+Pkg.Type_Declarations (TD.Array_Element_Type_Code).Name) &
                  ");");
               Put_Line
                 (File,
                  "subtype " &
                  (+TD.Name) &
                  " is Pkg_" &
                  (+TD.Name) &
                  ".Vector;");
               New_Line (File);
            when Struct_Kind =>
               Put_Line
                (File,
                 "type " &
                 (+TD.Name) &
                 " is record");

               for SM of TD.Struct_Members loop
                  Put_Line
                    (File, (+SM.Name) & " : " &
                    (+Pkg.Type_Declarations (SM.Type_Code).Name) & ";");
               end loop;

               Put_Line (File, "end record;");
               New_Line (File);
            when Dict_Kind =>
               Put_Line
                (File,
                 "package Pkg_" &
                 (+TD.Name) &
                 " is new Ada.Containers.Hashed_Maps (" &
                 (+Pkg.Type_Declarations (TD.Dict_Key_Type_Code).Name) &
                 ", " &
                 (+Pkg.Type_Declarations (TD.Dict_Element_Type_Code).Name) &
                 ");");
               Put_Line
                 (File,
                  "subtype " &
                  (+TD.Name) &
                  " is Pkg_" &
                  (+TD.Name) &
                  ".Map;");
               New_Line (File);
         end case;
      end loop;

      --  Print subprogram specs
      Put_Line (File, "------------------");
      Put_Line (File, "-- DBus Methods --");
      Put_Line (File, "------------------");
      for SP of Pkg.Subprograms loop
         New_Line (File);
         Print_Signature (SP, File);
         Put_Line (File, ";");
      end loop;

      Put_Line (File, "end " & (+Pkg.Name) & ";");
   end Print;
end Codegen.Specification;
