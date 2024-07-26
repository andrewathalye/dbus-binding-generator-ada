with Ada.Text_IO;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Doubly_Linked_Lists;
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

--  Basic Codegen
with Parsing;
with Codegen;
with Codegen.Types;

--  Client / Server Codegen
with Codegen.Client.Iface;
with Codegen.Server.Iface;

--  Utils
with Shared; use Shared;
with Debug;  use Debug;

procedure DBus_Binding_Generator_Ada is
   --------------------
   -- Instantiations --
   --------------------
   package File_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (String);
   use type Parsing.Node_Type;
   package Node_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Parsing.Node_Type);

   -----------------
   -- Subprograms --
   -----------------
   procedure Show_Help;
   procedure Show_Help is
      use Ada.Text_IO;
   begin
      Put_Line
        ("Usage: " & Ada.Command_Line.Command_Name &
         " [-client | -server] [input files]");
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
   File_List : File_Lists.List;
   Node_List : Node_Lists.List;
   Mode      : Client_Server := Client;

   ---------
   -- XML --
   ---------
   Grammar : Schema.Validators.XML_Grammar;
begin
   Put_Debug ("dbus_binding_generator_ada");

   ---------------
   -- Arguments --
   ---------------
   loop
      begin
         case GNAT.Command_Line.Getopt ("* client server help -help") is
            when 'c' =>
               Mode := Client;
            when 's' =>
               Mode := Server;
            when '*' =>
               File_List.Append (GNAT.Command_Line.Full_Switch);
            when ASCII.NUL =>
               exit;
            when others =>
               Show_Help;
         end case;
      exception
         when GNAT.Command_Line.Invalid_Switch =>
            GNAT.Command_Line.Try_Help;
            Error_Message ("Invalid switch");
            return;
      end;
   end loop;

   ------------
   -- Checks --
   ------------
   if File_List.Is_Empty then
      GNAT.Command_Line.Try_Help;
      Error_Message ("No input files specified");
   end if;

   --  Check that input files exist
   declare
      use type Ada.Directories.File_Kind;
   begin
      for File of File_List loop
         if Ada.Directories.Exists (File)
           and then Ada.Directories.Kind (File) = Ada.Directories.Ordinary_File
         then
            null;
         else
            Error_Message
              ("Input file " & File &
               " does not exist or is not an ordinary file.");
         end if;
      end loop;
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

   --------------------------------
   -- Parse all files into nodes --
   --------------------------------
   declare
      Input    : Input_Sources.File.File_Input;
      Reader   : Schema.Dom_Readers.Tree_Reader;
      Document : DOM.Core.Document;

      function Parse_File (Name : String) return Parsing.Node_Type;
      function Parse_File (Name : String) return Parsing.Node_Type is
         Result : Parsing.Node_Type;
      begin
         -----------------------------------
         -- Load document from input file --
         -----------------------------------
         declare
         begin
            Input_Sources.File.Open (Name, Input);
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
         Result :=
           Parsing.Process_Node (DOM.Core.Documents.Get_Element (Document));
         DOM.Core.Nodes.Free (Document);
         Put_Debug ("Parsed nodes for " & Name);

         return Result;
      end Parse_File;
   begin
      Reader.Set_Grammar (Grammar);
      Reader.Set_Feature (Sax.Readers.Schema_Validation_Feature, True);

      for File of File_List loop
         Node_List.Append (Parse_File (File));
      end loop;
   end;

   -------------------
   -- Generate code --
   -------------------
   declare
      Types_Pkg : Codegen.Types.Ada_Types_Package_Type;
      --  All collected type declarations

      procedure Recurse_Node (Node : in out Parsing.Node_Type);
      procedure Recurse_Node (Node : in out Parsing.Node_Type) is
      begin
         for Child_Node of Node.Child_Nodes loop
            Recurse_Node (Child_Node.all);
            Parsing.Free (Child_Node);
         end loop;

         for I of Node.Interfaces loop
            declare
               Pkg : constant Codegen.Ada_Package_Type :=
                 Codegen.Create_Package (I);
            begin
               Codegen.Types.Append_Types (Types_Pkg, Pkg);

               case Mode is
                  when Client =>
                     Codegen.Client.Iface.Print (Pkg);
                  when Server =>
                     Codegen.Server.Iface.Print (Pkg);
               end case;

               Put_Debug ("Generated interface " & (+I.Name));
            end;
         end loop;

         Put_Debug ("Generated all interfaces for node " & (+Node.Name));
      end Recurse_Node;
   begin
      for Top_Level_Node of Node_List loop
         Recurse_Node (Top_Level_Node);
      end loop;

      Codegen.Types.Print (Types_Pkg);
      Put_Debug ("Generated types package");
   end;
end DBus_Binding_Generator_Ada;
