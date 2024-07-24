with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Directories;

with Codegen.Output;
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

--  Basic Codegen
with Parsing;
with Codegen;

--  Client Codegen
with Codegen.Client.Node;
with Codegen.Client.Iface;

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
      Put_Line
        ("Usage: " & Ada.Command_Line.Command_Name &
         " [input file] [client|server]");
      GNAT.OS_Lib.OS_Exit (-1);
   end Show_Help;

   procedure Error_Message (Msg : String);
   procedure Error_Message (Msg : String) is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Msg);
      GNAT.OS_Lib.OS_Exit (-1);
   end Error_Message;

   -----------
   -- Types --
   -----------
   type Client_Server is (Client, Server);

   ---------------
   -- Variables --
   ---------------
   Input_File : Ada.Strings.Unbounded.Unbounded_String;
   Mode       : Client_Server;

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
      when 2 =>
         Input_File := +Ada.Command_Line.Argument (1);

         begin
            Mode := Client_Server'Value (Ada.Command_Line.Argument (2));
         exception
            when Constraint_Error =>
               Show_Help;
         end;
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
   Put_Debug ("Parsed nodes");

   -------------------
   -- Generate code --
   -------------------
   declare
      Types_Pkg : Codegen.Ada_Types_Package_Type;
      --  All collected type declarations

      procedure Recurse_Node (LN : in out Parsing.Node_Type);
      procedure Recurse_Node (LN : in out Parsing.Node_Type) is
      begin
         case Mode is
            when Client => null;
               Codegen.Client.Node.Print_Spec (LN);
               Codegen.Client.Node.Print_Body (LN);
            when Server =>
               raise Program_Error with "Server codegen unimplemented";
         end case;

         for N of LN.Child_Nodes loop
            Recurse_Node (N.all);
            Parsing.Free (N);
         end loop;

         for I of LN.Interfaces loop
            declare
               Pkg : constant Codegen.Ada_Package_Type :=
                 Codegen.Create_Package (LN.Name, I);
            begin
               Codegen.Append_Types (Types_Pkg, Pkg);

               case Mode is
                  when Client => null;
                     Codegen.Client.Iface.Print_Spec (Pkg);
                     Codegen.Client.Iface.Print_Body (Pkg);
                  when Server =>
                     raise Program_Error with "Server codegen unimplemented";
               end case;

               Put_Debug ("Generated interface " & (+I.Name));
            end;
         end loop;

         Put_Debug ("Generated node " & (+LN.Name));
      end Recurse_Node;
   begin
      Recurse_Node (Node);

      Codegen.Output.Declare_Types_Package (Types_Pkg);
      Put_Debug ("Generated types package");
   end;
end DBus_Binding_Generator_Ada;
