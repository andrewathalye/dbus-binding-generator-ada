with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Directories;

with GNAT.Command_Line;
with GNAT.OS_Lib;

--  xmlada
with Input_Sources.File;
with DOM.Core;
with DOM.Core.Nodes;
with DOM.Core.Documents;
with Sax.Readers;
with Schema.Dom_Readers;
with Schema.Schema_Readers;
with Schema.Validators;

--  Local Codegen
with Parsing;
with Codegen;
with Codegen.Specification;
with Codegen.The_Body;

--  Utils
with Shared; use Shared;
with Debug;  use Debug;

procedure DBus_Binding_Generator_Ada is
   -----------------
   -- Subprograms --
   -----------------
   procedure Show_Help;
   procedure Show_Help is
      use Ada.Text_IO;
   begin
      Put_Line ("Usage: " & Ada.Command_Line.Command_Name & " [INPUT]");
      GNAT.OS_Lib.OS_Exit (-1);
   end Show_Help;

   procedure Error_Message (Msg : String);
   procedure Error_Message (Msg : String) is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Msg);
      GNAT.OS_Lib.OS_Exit (-1);
   end Error_Message;

   ---------------
   -- Variables --
   ---------------
   Input_File : Ada.Strings.Unbounded.Unbounded_String;

   ---------
   -- XML --
   ---------
   Grammar  : Schema.Validators.XML_Grammar;
   Document : DOM.Core.Document;
   Node     : Parsing.Node_Type;
begin
   Put_Debug ("dbus_binding_generator_ada");

   ---------------
   -- Arguments --
   ---------------
   loop
      begin
         case GNAT.Command_Line.Getopt ("h help -help") is
            when ASCII.NUL =>
               exit;
            when others =>
               Show_Help;
         end case;
      exception
         when GNAT.Command_Line.Invalid_Switch =>
            GNAT.Command_Line.Try_Help;
      end;
   end loop;

   ------------
   -- Checks --
   ------------
   --  Load input file
   case Ada.Command_Line.Argument_Count is
      when 1 =>
         Input_File := +Ada.Command_Line.Argument (1);
      when others =>
         Show_Help;
   end case;

   --  Check that these exist
   declare
      use type Ada.Directories.File_Kind;
   begin
      if Ada.Directories.Exists (+Input_File)
        and then Ada.Directories.Kind (+Input_File) =
          Ada.Directories.Ordinary_File
      then
         null;
      else
         Error_Message
           ("Input file " & (+Input_File) &
            " does not exist or is not a file.");
      end if;
   end;

   -----------------
   -- Load schema --
   -----------------
   declare
      Input  : Input_Sources.File.File_Input;
      Reader : Schema.Schema_Readers.Schema_Reader;
   begin
      Input_Sources.File.Open ("data/introspect.xsd", Input);
      Reader.Parse (Input);
      Input.Close;
      Grammar := Reader.Get_Grammar;
      Reader.Free;
   exception
      when Schema.Validators.XML_Validation_Error =>
         Error_Message ("ERROR: " & Reader.Get_Error_Message);

   end;
   Put_Debug ("Loaded grammar");

   -----------------------------------
   -- Load document from input file --
   -----------------------------------
   declare
      Input  : Input_Sources.File.File_Input;
      Reader : Schema.Dom_Readers.Tree_Reader;
   begin
      Reader.Set_Grammar (Grammar);
      Reader.Set_Feature (Sax.Readers.Schema_Validation_Feature, True);

      Input_Sources.File.Open (+Input_File, Input);
      Reader.Parse (Input);
      Input.Close;
      Document := Reader.Get_Tree;
      Reader.Free;
   exception
      when Schema.Validators.XML_Validation_Error =>
         Error_Message ("ERROR: " & Reader.Get_Error_Message);
   end;
   Put_Debug ("Loaded tree");

   ----------------------
   -- Process document --
   ----------------------
   --  Note: The document must be valid by definition
   Node := Parsing.Process_Node (DOM.Core.Documents.Get_Element (Document));
   DOM.Core.Nodes.Free (Document);

   -------------------
   -- Generate code --
   -------------------
   for I of Node.Interfaces loop
      declare
         Pkg : constant Codegen.Ada_Package_Type :=
           Codegen.Create_Package (Node.Name, I);
      begin
         Codegen.Specification.Print (Pkg);
         Codegen.The_Body.Print (Pkg);
      end;
   end loop;
end DBus_Binding_Generator_Ada;
